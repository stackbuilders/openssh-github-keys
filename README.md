[![Build Status](https://travis-ci.org/stackbuilders/openssh-github-keys.svg?branch=master)](https://travis-ci.org/stackbuilders/openssh-github-keys) [![Hackage](https://img.shields.io/hackage/v/openssh-github-keys.svg)](http://hackage.haskell.org/package/openssh-github-keys)

# openssh-github-keys

Your developers are organized in GitHub teams, and GitHub has
everyone's public key. Why are you still manually editing the
`authorized_keys` file on your servers?

## Operating Details

Newer versions of OpenSSH have an option that allows you to pull a
list of authorized keys for a user. This command pulls keys GitHub and
OpenSSH allows login using the selected user accounts.

## Usage

First, you need to install the `openssh-github-keys` command:

```
sudo apt-get install haskell-platform
cabal install openssh-github-keys --global
```

This should install the binary `openssh-github-keys` under
/usr/local/bin.

Generate an application token which has read-only organization
access. This will let the application read your teams and members
(public keys have always been public).

The OpenSSH option `AuthorizedKeysCommand` cannot have any arguments
specified following the command, and we need to pass options to
specify which organization and team is used, as well as which user
should be authenticated using keys in GitHub, so we will create a
wrapper script for openssh-github-keys. Additionally
`openssh-github-keys` will need your `GITHUB_TOKEN` which may be
specified as a variable in the wrapper script, or in a
[dotenv file](https://github.com/stackbuilders/dotenv-hs). Your
wrapper script may look like the one below, which you could create as
`/usr/local/bin/openssh-github-keys-wrapper`:

```bash
#!/bin/bash

/usr/local/bin/openssh-github-keys \
    -o your-github-org \
    -t your-github-team \
    -u user-to-auth-with-github \
    -f /path/to/your/github-token-dotenv-file \
    $1
```

Make sure this script is executable and owned by the `root` user (`chmod 755
/usr/local/bin/openssh-github-keys-wrapper`).

In your `/etc/ssh/sshd_config`, add the option for
AuthorizedKeysCommand to point to your wrapper script. You will also
need to specify the user to run the script:

```
AuthorizedKeysCommand /usr/local/bin/openssh-github-keys-wrapper
AuthorizedKeysUser root
```

The `openssh-github-keys` script will need to know your GitHub
token. You can specify this as a variable `GITHUB_TOKEN` in your
wrapper script, or point to a
file containing
this token.

## Availability

`openssh-github-keys` takes precautions to ensure that you don't lose
access to servers even if GitHub is unavailable or slow.

### GitHub unreachable

It may be a concern that if GitHub goes down, certain users will not
have access to your servers. To prevent this dependency, you may want
certain users to be specified in the ~/.ssh/authorized_keys file. This
file is used as a fallback if the AuthorizedKeysCommand does not
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


## License

MIT

## Author

[Stack Builders](http://www.stackbuilders.com/)
