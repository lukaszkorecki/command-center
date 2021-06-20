# Useful Postgres stuff

List all connected clients:

```sql
select pid as process_id,
       usename as username,
       datname as database_name,
       client_addr as client_address,
       application_name,
       backend_start,
       state,
       state_change
from pg_stat_activity;

```

then kill them (useful when there are hanging DB connections):

```sql

SELECT
	pg_terminate_backend(pg_stat_activity.pid)
FROM
	pg_stat_activity
WHERE
	pg_stat_activity.datname like '%test%'
	AND pid <> pg_backend_pid();

 ```


# Switching databases in `psql`

`\c dbname`
