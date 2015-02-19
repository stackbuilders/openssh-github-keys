[![Build Status](https://travis-ci.org/stackbuilders/openssh-github-keys.svg?branch=master)](https://travis-ci.org/stackbuilders/openssh-github-keys) [![Hackage](https://img.shields.io/hackage/v/openssh-github-keys.svg)](http://hackage.haskell.org/package/openssh-github-keys)

# openssh-github-keys

You already have developers organized in Github teams, and Github
knows the public keys of everyone on your team. Why not use Github to
control access to your servers via SSH?

## Operating Details

Newer versions of OpenSSH have an option that allows you to pull a
list of authorized keys for a user. This command pulls keys Github and
OpenSSH allows login using the selected user accounts.

## Usage

```
sudo apt-get install haskell-platform --global
cabal install openssh-github-keys
```

Generate an application token which has read-only organization
access. This will let the application read your teams and members
(public keys have always been public).

In your `/etc/ssh/sshd_config`, change the following line:

```
AuthorizedKeysCommand /usr/local/bin/openssh-github-keys -o your-github-org -t your-github-team -u username
```

You will need to provide `GITHUB_TOKEN` as an environment variable to
the above command.

## Security Notes

Obviously, using this configuration means that your servers are only
as secure as your Github organization. You should consider adding
things like two-factor authentication for accounts which can modify
your Github organization.

## License

MIT

## Author

[Stack Builders](http://www.stackbuilders.com/)
