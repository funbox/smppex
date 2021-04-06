#!/bin/bash

openssl req \
    -x509 \
    -new \
    -nodes \
    -key ca.key \
    -subj "/C=RU/ST=Moscow/O=RubyBox/CN=rubybox.ru" \
    -sha256 \
    -days 10000 \
    -out ca.crt

openssl req \
    -batch \
    -new \
    -key cert.key \
    -subj '/CN=localhost/O=SMPPEX/C=RU/ST=Moscow/L=Moscow' \
    -out localhost.csr

openssl x509 \
    -req \
    -in localhost.csr \
    -days 10000 \
    -CA ca.crt \
    -CAkey ca.key \
    -CAcreateserial \
    -out localhost.crt

openssl req \
    -batch \
    -new \
    -key cert.key \
    -subj '/CN=bad-hostname/O=SMPPEX/C=RU/ST=Moscow/L=Moscow' \
    -out badhost.csr

openssl x509 \
    -req \
    -in badhost.csr \
    -days 10000 \
    -CA ca.crt \
    -CAkey ca.key \
    -CAcreateserial \
    -out badhost.crt
