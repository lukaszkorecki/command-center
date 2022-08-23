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

# CTEs

Because I always forget the syntax:


```sql

with
  some_stuff as (select id from accounts where created_at < now() - interval '7 days'),
  more_stuff as (select id from users where account_id in (select * from some_stuff))

select teams where account_id in (select * from some_stuff) and user_id in (select * from more_stuff)
```
