#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: "
    echo "./decrypt.sh <config-folder>"
    exit 1;
fi

mkdir -p "/tmp/$1/" && \
  cd "/home/moritz/.gnupg/$1/" && \
  find . -type f | \
  cut -d"/" -f2 | \
  xargs -I file sh -c "gpg --decrypt file > /tmp/$1/file" &&\
  cd "/tmp/$1/" &&
  rename 's/\.gpg$//' *.gpg &&\
  find . -type f | \
  xargs chmod 400
