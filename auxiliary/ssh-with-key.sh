#!/bin/sh

cat ~/.ssh/id_rsa.pub | ssh git@192.168.2.101 'cat >> .ssh/authorized_keys'

