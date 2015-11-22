# PureScript Yelp App

Small app built with PureScript, Node.js, and the Yelp API.

## Quickstart

Make sure you have [PureScript](http://www.purescript.org/) and [Pulp](https://github.com/bodil/pulp) installed.

Clone this repository and install dependencies with:

```bash
$ scripts/install.sh
```

You will need [Yelp API access](https://www.yelp.com/developers/manage_api_keys) to run the backend.

In the root directory, create `.env` file to load your Yelp API credentials:

```bash
export CONSUMER_KEY='...'
export CONSUMER_SECRET='...'
export TOKEN='...'
export TOKEN_SECRET='...'
```

In a first terminal run:

```bash
$ scripts/build-dev.sh
```

In a second terminal run:

```bash
$ scripts/run-dev.sh
```

Navigate to `http://localhost:3000`.
