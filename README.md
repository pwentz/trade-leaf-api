## Trade Leaf API


This is the JSON API for the [tradeLeaf mobile app](https://github.com/pwentz/tradeLeaf).


## Getting Started

To build this repo, you need to download [stack](https://docs.haskellstack.org/en/stable/README/) if you haven't already.

```
stack build
```

This app uses a Postgresql database for storage, so you need a `trade_leaf` Postgresql database that shares the same user
as your environment.

With the database in place, you need to install `moo-postgresql` in order to manage database migrations.

```
stack install moo-postgresql
```

Then run the following command to run all migrations.
*Note* make sure that you have the environment variables needed for these migrations.

```
moo-postgresql upgrade
```

With the project built, the database setup, and the migrations ran, you can now start the server.

```
stack exec trade-leaf
```


## Environment Variables

In order for this app to work as expected, there needs to be a number of environment variables available to the application.

  - `TRADE_LEAF_SECRET` - this is the sha used for encryption when building JSON Web Tokens for authentication.
  - `DBM_MIGRATION_STORE` - this is the absolute path of the `migrations` folder where the migrations are stored.
  - `DBM_DATABASE` - this is the connection string for the database


## Testing

In order to run the tests, you need to have a `trade_leaf_test` Postgresql database that shares the same user as your environment.

Once the test database is in place, the following command will run all the tests.

```
stack test
```
