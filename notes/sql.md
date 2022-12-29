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

WITH
  some_stuff AS (SELECT id FROM accounts WHERE created_at < now() - INTERVAL '7 days'),
  more_stuff AS (SELECT id FROM users WHERE account_id IN (SELECT * FROM some_stuff))

SELECT teams WHERE account_id IN (SELECT * FROM some_stuff) AND user_id IN (SELECT * FROM more_stuff)
```


# `FILTER`


```sql
SELECT
  COUNT(*) as "All Users",
  COUNT (*) FILTER (WHERE u. verified) AS "Verified Users",
  COUNT(*) FILTER (WHERE NOT u. verified) AS "Unverified Users"

FROM users AS u;
```


From here: https://twitter.com/PJUllrich/status/1595069913336803329
