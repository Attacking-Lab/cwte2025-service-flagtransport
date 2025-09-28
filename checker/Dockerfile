FROM ubuntu:25.04
ENV DEBIAN_FRONTEND=noninteractive
ADD https://raw.githubusercontent.com/reproducible-containers/repro-sources-list.sh/d15cf12b26395b857b24fba223b108aff1c91b26/repro-sources-list.sh /var/lib/repro-sources-list.sh
RUN \
  --mount=type=cache,target=/var/cache/apt,sharing=locked \
  --mount=type=cache,target=/var/lib/apt,sharing=locked \
  bash /var/lib/repro-sources-list.sh && \
  apt-get update && \
  apt-get install -y python3 python3-pip git && \
  rm -rf /var/lib/repro-sources-list.sh /var/log/* /var/cache/ldconfig/aux-cache

WORKDIR /tmp

RUN useradd -ms /bin/bash checker
RUN python3 -m pip install --break-system-packages uv git-pass-helper==0.2.1

WORKDIR /checker
RUN chown checker -R /checker
USER checker

COPY ./src/pyproject.toml ./src/uv.lock /checker/

RUN --mount=type=secret,id=github-pat,env=GITHUB_PAT \
  git-pass -p "${GITHUB_PAT:?GITHUB PAT not set}" -- \
    uv sync --exact --frozen

COPY ./src/*.py /checker/

ENTRYPOINT [ "uv", "run", "--no-sync", "gunicorn", "-c", "gunicorn.conf.py", "checker:app" ]
