[![Build Status](https://travis-ci.org/stackbuilders/openssh-github-keys.svg?branch=master)](https://travis-ci.org/stackbuilders/openssh-github-keys) [![Hackage](https://img.shields.io/hackage/v/openssh-github-keys.svg)](http://hackage.haskell.org/package/openssh-github-keys)

# openssh-github-keys

Your developers are organized in GitHub teams, and GitHub has
everyone's public key. Why are you still manually editing the
`authorized_keys` file on your servers?

# Stability

Experimental. This program has not yet been subjected to production
testing. Feedback and pull requests are welcome.

## Operating Details

Newer versions of OpenSSH have an option that allows you to pull a
list of authorized keys for a user. This command pulls keys GitHub and
OpenSSH allows login using the selected user accounts.

## Usage

First, you need to install the `openssh-github-keys` tool:

```bash
$ curl -sSL https://get.haskellstack.org/ | sh
$ stack setup
$ stack install openssh-github-keys
```

This should install the binary `openssh-github-keys` under `~/.local/bin`.

Generate an application token which has read-only organization
access. This will let the application read your teams and members
(SSH public keys added by users have always been public on GitHub).

The OpenSSH option `AuthorizedKeysCommand` cannot have any arguments
specified following the command, and we need to pass options to
specify which organization and team is used, as well as which user
should be authenticated using keys in GitHub, so we will create a
configuration file for `openssh-github-keys`. Additionally
`openssh-github-keys` will need your `GITHUB_TOKEN` which must be
specified inside the `/etc/openssh-github-keys/github.creds` file, or
on the command line. Your configuration file may look like the one below,
which you create as `/etc/openssh-github-keys/settings.yaml`:

```yaml
organization: your-github-org
team: your-github-team
```

The `openssh-github-keys` script will need to know your GitHub
token. You can specify this as a variable `GITHUB_TOKEN` in your
`/etc/openssh-github-keys/github.creds` configuration file, the file
format should contain a key, `GITHUB_TOKEN` and the value as follows:

```
GITHUB_TOKEN=mygithubtoken
```

Make sure this file is owned by the `nobody` user (`chmod 600
/etc/openssh-github-keys/github.creds`).

At this point you should be able to test that the command is properly
configured. Invoke the command with the user you intend to log in as
using keys on a GitHub team:

```bash
# Print list of keys, corresponding to the GitHub users on your team
openssh-github-keys mylocaluser
```

Note that if you pass any user that is not specified in the configuration
as a user to log in using GitHub keys, the command will return immediately.
This will allow other users to log in only using locally-configured keys,
and without any delay induced by network communication to GitHub's API.

In your `/etc/ssh/sshd_config`, add the option for `AuthorizedKeysCommand`
to point to your wrapper script. You will also need to specify the user to
run the script:

```bash
AuthorizedKeysCommand openssh-github-keys
AuthorizedKeysCommandUser nobody
```

You should test the syntax of your sshd_config file by using `sshd
-t`. Then, if all is well, restart the ssh service with `service ssh restart`.

## Troubleshooting

The following troubleshooting steps are recommended if you have issues
using openssh-github-keys:

* Make sure that you can invoke openssh-github-keys-wrapper, passing
  the user to authenticate as the first argument. This should return a
  list of valid SSH public keys that can be used for authentication.
* Check the output of /var/log/auth.log to see why login may be
  failing.
* Change the sshd log level, by changing the line in `sshd_config` for
  `LogLevel` to `DEBUG`. Restart the sshd service with `service ssh
  restart` or the equivalent on your platform.

## `openssh-github-keys` and GitHub Failure Conditions

`openssh-github-keys` takes precautions to ensure that you don't lose
access to servers even if GitHub is unavailable or slow. Below we
describe the different types of failure that we've anticipated at
GitHub, and how openssh-github-keys should behave when these
conditions are detected.

### GitHub unreachable

It may be a concern that if GitHub goes down, certain users will not
have access to your servers. To prevent this dependency, you may want
certain users to be specified in the `~/.ssh/authorized_keys` file. This
file is used as a fallback if the `AuthorizedKeysCommand` does not
succeed.

### GitHub latency

`openssh-github-keys` will time out if keys are not able to be
retrieved in five seconds. This allows other authentication
mechanisms, such as the authorized_keys file to be consulted so that
login can proceed for certain users even in the event of an unusual
amount of latency while communicating with GitHub.

## Security

Using a tool such as `openssh-github-keys` this configuration means
that your servers are only as secure as GitHub, and your GitHub
organization. You should consider adding things like two-factor
authentication for accounts which can modify your Github
organization. If you cannot afford to have your servers compromised in
the event of a major security breach at GitHub you should not use
`openssh-github-keys`.

## Precautionary Installation Suggestions

Since `openssh-github-keys` is just an experimental library, you may
want to have a user account that relies on a standard authorized_keys
file for a small group of primary users (e.g., system administrators)
and give the rest of your team access through the GitHub
authentication mechanism.

## License

MIT

## Author

[Stack Builders](http://www.stackbuilders.com/)
