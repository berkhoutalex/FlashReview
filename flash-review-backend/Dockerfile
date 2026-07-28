# syntax=docker/dockerfile:1

# --- build stage ---
# GHC 9.8.4 matches the compiler in stack snapshot lts-23.26.
FROM haskell:9.8.4 AS build

WORKDIR /app

# libpq-dev is required to link postgresql-simple; zlib1g-dev for http-client.
# The haskell:9.8.4 image is Debian bullseye, whose libpq-dev is 13.x, but the
# lts-23.26 snapshot's postgresql-libpq-configure requires libpq >= 14.12, so we
# pull a current libpq-dev from the PostgreSQL APT repository (PGDG).
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl gnupg \
    && install -d /usr/share/postgresql-common/pgdg \
    && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
         -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
    && echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" \
         > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update && apt-get install -y --no-install-recommends \
      libpq-dev \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy only the dependency manifests first so this layer caches across source
# changes. Haskell dependency builds are slow and Render has no build cache.
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --system-ghc --only-dependencies

COPY . .
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin

# --- runtime stage ---
FROM debian:bookworm-slim

# libpq5 is the runtime half of libpq-dev; without it the server fails to start.
# Pull it from PGDG so the runtime libpq major matches what the binary was built
# against (bookworm ships 15; the build stage links the current PGDG libpq).
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl gnupg \
    && install -d /usr/share/postgresql-common/pgdg \
    && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
         -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
    && echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bookworm-pgdg main" \
         > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update && apt-get install -y --no-install-recommends \
      libgmp10 \
      libpq5 \
      zlib1g \
    && apt-get purge -y --auto-remove curl gnupg \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/bin/flash-review-backend-exe /app/server

ENV PORT=10000
EXPOSE 10000

CMD ["/app/server"]
