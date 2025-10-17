#!/bin/bash

POSTGRES_DB="postgres"
POSTGRES_USER="postgres"
POSTGRES_PASSWORD="password"
PGDATA="./data"

MAX_CONNECTIONS="1000"
SHARED_BUFFERS="256MB"
EFFECTIVE_CACHE_SIZE="768MB"
MAINTENANCE_WORK_MEM="64MB"
WAL_BUFFERS="16MB"
DEFAULT_STATISTICS_TARGET="100"

mkdir -p "$PGDATA"

if [ ! -d "$PGDATA/base" ]; then
    initdb -D "$PGDATA" --username="$POSTGRES_USER" --pwfile=<(echo "$POSTGRES_PASSWORD")
else
    echo "Database cluster already exists."
fi

cat <<EOF > "$PGDATA/postgresql.conf.tmp"
max_connections = $MAX_CONNECTIONS
shared_buffers = $SHARED_BUFFERS
effective_cache_size = $EFFECTIVE_CACHE_SIZE
maintenance_work_mem = $MAINTENANCE_WORK_MEM
wal_buffers = $WAL_BUFFERS
default_statistics_target = $DEFAULT_STATISTICS_TARGET
EOF

cat "$PGDATA/postgresql.conf.tmp" >> "$PGDATA/postgresql.conf"
rm "$PGDATA/postgresql.conf.tmp"

pg_ctlcluster 17 main start -- -D "$PGDATA"

PG_PID=$!

until pg_isready -U "$POSTGRES_USER" -d "$POSTGRES_DB" &> /dev/null; do
    sleep 1
done

PGPASSWORD="$POSTGRES_PASSWORD" psql -U "$POSTGRES_USER" -d postgres -c "CREATE DATABASE $POSTGRES_DB;" 2>/dev/null || echo "Database '$POSTGRES_DB' already exists or could not be created."