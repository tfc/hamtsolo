# hamtsolo

[![Build Status](https://travis-ci.org/tfc/hamtsolo.svg?branch=master)](https://travis-ci.org/tfc/hamtsolo)

Connect to an Intel computer with enabled AMT and establish a serial-over-lan (SOL) connection.

Does not support encryption. (Use Intel ME in small business mode.)

## Usage

```bash
hamtsolo - An Intel AMT Serial-Over-LAN (SOL) client

Usage: hamtsolo [-u|--user <user>] [-p|--pass <password>] [--port <port>] <host>
  hamtsolo lets you connect to Intel computers with enabled AMT and establish a
  serial-over-lan (SOL) connection.

Available options:
  -h,--help                Show this help text
  -u,--user <user>         Authentication user name (default: "admin")
  -p,--pass <password>     Authentication password (default: "Password123!")
  --port <port>            TCP connection port (default: 16994)
  <host>                   AMT host to connect to
```
