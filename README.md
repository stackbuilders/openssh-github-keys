[![Build Status](https://travis-ci.org/stackbuilders/openssh-github-keys.svg?branch=master)](https://travis-ci.org/stackbuilders/openssh-github-keys) [![Hackage](https://img.shields.io/hackage/v/openssh-github-keys.svg)](http://hackage.haskell.org/package/openssh-github-keys)

# openssh-github-keys

You already have developers organized in GitHub teams, and GitHub
knows the public keys of everyone on your team. Why not use GitHub to
control access to your servers via SSH?

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

Generate an application token which has read-only organization
access. This will let the application read your teams and members
(public keys have always been public).

The OpenSSH `AuthorizedKeysCommand` cannot have any options specified
(it is passed as an argument the user trying to log in), and we need
to configure the command, so we should create a wrapper script
containing the options for selecting the users to allow. Additionally
`openssh-github-keys` will need your `GITHUB_TOKEN` which may be
specified as a variable in the wrapper script, or in a
[dotenv file](https://github.com/stackbuilders/dotenv-hs).

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

## Availability Notes

It may be a concern that if GitHub goes down, certain users will not
have access to your servers. To prevent this dependency, you may want
certain users to be specified in the ~/.ssh/authorized_keys file. This
file is used as a fallback if the AuthorizedKeysCommand does not
succeed.

## Security Notes

Obviously, using this configuration means that your servers are only
as secure as your Github organization. You should consider adding
things like two-factor authentication for accounts which can modify
your Github organization.

## License

MIT

## Author

[Stack Builders](http://www.stackbuilders.com/)
