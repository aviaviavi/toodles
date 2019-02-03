#!/usr/bin/env python

import sys
from Crypto.PublicKey import RSA
from Crypto.Signature import PKCS1_v1_5
from Crypto.Hash import SHA512
from base64 import b64decode, b64encode

def verify_sign(public_key_loc, signature, data):
    pub_key = open(public_key_loc, "r").read()
    rsakey = RSA.importKey(pub_key)
    signer = PKCS1_v1_5.new(rsakey)
    digest = SHA512.new()
    digest.update(data)
    if signer.verify(digest, b64decode(signature)):
        return True
    return False

if __name__ == "__main__":
    print(verify_sign(sys.argv[1], sys.argv[2], sys.argv[3]))

