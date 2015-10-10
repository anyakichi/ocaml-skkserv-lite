#!/usr/bin/env python
# -*- coding: utf-8 -*-

import socket

host = 'localhost'
port = 1178

## connect and disconnect
for i in range(100000):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((host, port))
    sock.send('1a \n'.encode())
    sock.recv(4096)
    sock.close()


## connect from 100 clients
sockets = []

for i in range(100):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((host, port))
    sock.send('1a \n'.encode())
    sock.recv(4096)
    sockets.append(sock)

for i in range(100):
    for sock in sockets:
        sock.send(u'1あk \n'.encode('euc-jp'))
        sock.recv(4096)

for sock in sockets:
    sock.close()


## create a skk-jisyo database and check the content
# TODO
