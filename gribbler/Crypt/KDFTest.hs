-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/KDFTest.hs: Unit tests for KDF
-- Copyright (C) 2021-2024 LStandman

module Crypt.KDFTest(
    test_hmac_sha256,
    test_pbkdf2_hmac_sha256)
  where

import qualified Crypt.KDF as KDF
import qualified Crypt.SHA2 as SHA2
import Libtest
import Misc.MemUtils

test_hmac_sha256 =
  let
    --  From draft-ietf-ipsec-ciph-sha-256-01.txt
    t01_key    = [
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
      0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
      0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
      0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20]
    t01_ksize  = 32
    t01_ptext  = strBytes "abc"
    t01_psize  = 3
    t01_digest = [
      0xA2, 0x1B, 0x1F, 0x5D, 0x4C, 0xF4, 0xF7, 0x3A,
      0x4D, 0xD9, 0x39, 0x75, 0x0F, 0x7A, 0x06, 0x6A,
      0x7F, 0x98, 0xCC, 0x13, 0x1C, 0xB1, 0x6A, 0x66,
      0x92, 0x75, 0x90, 0x21, 0xCF, 0xAB, 0x81, 0x81]
    t02_key    = [
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
      0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
      0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
      0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20]
    t02_ksize  = 32
    t02_ptext  =
      strBytes "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    t02_psize  = 56
    t02_digest = [
      0x10, 0x4F, 0xDC, 0x12, 0x57, 0x32, 0x8F, 0x08,
      0x18, 0x4B, 0xA7, 0x31, 0x31, 0xC5, 0x3C, 0xAE,
      0xE6, 0x98, 0xE3, 0x61, 0x19, 0x42, 0x11, 0x49,
      0xEA, 0x8C, 0x71, 0x24, 0x56, 0x69, 0x7D, 0x30]
    t03_key    = [
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
      0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
      0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
      0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20]
    t03_ksize  = 32
    t03_ptext  = strBytes (
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ++
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    t03_psize  = 112
    t03_digest = [
      0x47, 0x03, 0x05, 0xFC, 0x7E, 0x40, 0xFE, 0x34,
      0xD3, 0xEE, 0xB3, 0xE7, 0x73, 0xD9, 0x5A, 0xAB,
      0x73, 0xAC, 0xF0, 0xFD, 0x06, 0x04, 0x47, 0xA5,
      0xEB, 0x45, 0x95, 0xBF, 0x33, 0xA9, 0xD1, 0xA3]
    t04_key    = [
      0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
      0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
      0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
      0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B]
    t04_ksize  = 32
    t04_ptext  = strBytes "Hi There"
    t04_psize  = 8
    t04_digest = [
      0x19, 0x8A, 0x60, 0x7E, 0xB4, 0x4B, 0xFB, 0xC6,
      0x99, 0x03, 0xA0, 0xF1, 0xCF, 0x2B, 0xBD, 0xC5,
      0xBA, 0x0A, 0xA3, 0xF3, 0xD9, 0xAE, 0x3C, 0x1C,
      0x7A, 0x3B, 0x16, 0x96, 0xA0, 0xB6, 0x8C, 0xF7]
    t05_key    = strBytes "Jefe"
    t05_ksize  = 4
    t05_ptext  = strBytes "what do ya want for nothing?"
    t05_psize  = 28
    t05_digest = [
      0x5B, 0xDC, 0xC1, 0x46, 0xBF, 0x60, 0x75, 0x4E,
      0x6A, 0x04, 0x24, 0x26, 0x08, 0x95, 0x75, 0xC7,
      0x5A, 0x00, 0x3F, 0x08, 0x9D, 0x27, 0x39, 0x83,
      0x9D, 0xEC, 0x58, 0xB9, 0x64, 0xEC, 0x38, 0x43]
    t06_key    = [
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA
      ]
    t06_ksize  = 32
    t06_ptext  = [
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
      0xDD, 0xDD]
    t06_psize  = 50
    t06_digest = [
      0xCD, 0xCB, 0x12, 0x20, 0xD1, 0xEC, 0xCC, 0xEA,
      0x91, 0xE5, 0x3A, 0xBA, 0x30, 0x92, 0xF9, 0x62,
      0xE5, 0x49, 0xFE, 0x6C, 0xE9, 0xED, 0x7F, 0xDC,
      0x43, 0x19, 0x1F, 0xBD, 0xE4, 0x5C, 0x30, 0xB0]
    t07_key    = [
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
      0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
      0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
      0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
      0x21, 0x22, 0x23, 0x24, 0x25]
    t07_ksize  = 37
    t07_ptext  = [
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD, 0xCD,
      0xCD, 0xCD]
    t07_psize  = 50
    t07_digest = [
      0xD4, 0x63, 0x3C, 0x17, 0xF6, 0xFB, 0x8D, 0x74,
      0x4C, 0x66, 0xDE, 0xE0, 0xF8, 0xF0, 0x74, 0x55,
      0x6E, 0xC4, 0xAF, 0x55, 0xEF, 0x07, 0x99, 0x85,
      0x41, 0x46, 0x8E, 0xB4, 0x9B, 0xD2, 0xE9, 0x17]
    t08_key    = [
      0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C,
      0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C,
      0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C,
      0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C]
    t08_ksize  = 32
    t08_ptext  = strBytes "Test With Truncation"
    t08_psize  = 20
    t08_digest = [
      0x75, 0x46, 0xAF, 0x01, 0x84, 0x1F, 0xC0, 0x9B,
      0x1A, 0xB9, 0xC3, 0x74, 0x9A, 0x5F, 0x1C, 0x17,
      0xD4, 0xF5, 0x89, 0x66, 0x8A, 0x58, 0x7B, 0x27,
      0x00, 0xA9, 0xC9, 0x7C, 0x11, 0x93, 0xCF, 0x42]
    t09_key    = [
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA]
    t09_ksize  = 80
    t09_ptext  =
      strBytes "Test Using Larger Than Block-Size Key - Hash Key First"
    t09_psize  = 54
    t09_digest = [
      0x69, 0x53, 0x02, 0x5E, 0xD9, 0x6F, 0x0C, 0x09,
      0xF8, 0x0A, 0x96, 0xF7, 0x8E, 0x65, 0x38, 0xDB,
      0xE2, 0xE7, 0xB8, 0x20, 0xE3, 0xDD, 0x97, 0x0E,
      0x7D, 0xDD, 0x39, 0x09, 0x1B, 0x32, 0x35, 0x2F]
    t10_key    = [
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
      0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA]
    t10_ksize  = 80
    t10_ptext  = strBytes (
      "Test Using Larger Than Block-Size Key and Larger Than " ++
      "One Block-Size Data")
    t10_psize  = 73
    t10_digest = [
      0x63, 0x55, 0xAC, 0x22, 0xE8, 0x90, 0xD0, 0xA3,
      0xC8, 0x48, 0x1A, 0x5C, 0xA4, 0x82, 0x5B, 0xC8,
      0x84, 0xD3, 0xE7, 0xA1, 0xFF, 0x98, 0xA2, 0xFC,
      0x2A, 0xC7, 0xD8, 0xE0, 0x64, 0xC3, 0xB2, 0xE6]
  in
    testsuite "HMACxSHA256" [
      test "IETFDraft003" [
        expect_memeq "t01_digest" t01_digest $
          KDF.hmac_sha256 t01_key t01_ksize t01_ptext t01_psize],
      test "IETFDraft056" [
        expect_memeq "t02_digest" t02_digest $
          KDF.hmac_sha256 t02_key t02_ksize t02_ptext t02_psize],
      test "IETFDraft112" [
        expect_memeq "t03_digest" t03_digest $
          KDF.hmac_sha256 t03_key t03_ksize t03_ptext t03_psize],
      test "IETFDraft008" [
        expect_memeq "t04_digest" t04_digest $
          KDF.hmac_sha256 t04_key t04_ksize t04_ptext t04_psize],
      test "IETFDraft028" [
        expect_memeq "t05_digest" t05_digest $
          KDF.hmac_sha256 t05_key t05_ksize t05_ptext t05_psize],
      test "IETFDraft050x32" [
        expect_memeq "t06_digest" t06_digest $
          KDF.hmac_sha256 t06_key t06_ksize t06_ptext t06_psize],
      test "IETFDraft050x37" [
        expect_memeq "t07_digest" t07_digest $
          KDF.hmac_sha256 t07_key t07_ksize t07_ptext t07_psize],
      test "IETFDraft020" [
        expect_memeq "t08_digest" t08_digest $
          KDF.hmac_sha256 t08_key t08_ksize t08_ptext t08_psize],
      test "IETFDraft054" [
        expect_memeq "t09_digest" t09_digest $
          KDF.hmac_sha256 t09_key t09_ksize t09_ptext t09_psize],
      test "IETFDraft073" [
        expect_memeq "t10_digest" t10_digest $
          KDF.hmac_sha256 t10_key t10_ksize t10_ptext t10_psize]]

test_pbkdf2_hmac_sha256 =
  let
    --  From RFC 7914.
    t1_pass    = strBytes "passwd"
    t1_psize   = 6
    t1_salt    = strBytes "salt"
    t1_ssize   = 4
    t1_c       = 1
    t1_dk_len  = 64
    t1_derived = [
      0x55, 0xAC, 0x04, 0x6E, 0x56, 0xE3, 0x08, 0x9F,
      0xEC, 0x16, 0x91, 0xC2, 0x25, 0x44, 0xB6, 0x05,
      0xF9, 0x41, 0x85, 0x21, 0x6D, 0xDE, 0x04, 0x65,
      0xE6, 0x8B, 0x9D, 0x57, 0xC2, 0x0D, 0xAC, 0xBC,
      0x49, 0xCA, 0x9C, 0xCC, 0xF1, 0x79, 0xB6, 0x45,
      0x99, 0x16, 0x64, 0xB3, 0x9D, 0x77, 0xEF, 0x31,
      0x7C, 0x71, 0xB8, 0x45, 0xB1, 0xE3, 0x0B, 0xD5,
      0x09, 0x11, 0x20, 0x41, 0xD3, 0xA1, 0x97, 0x83]
    t2_pass    = strBytes "Password"
    t2_psize   = 8
    t2_salt    = strBytes "NaCl"
    t2_ssize   = 4
    t2_c       = 80000
    t2_dk_len  = 64
    t2_derived = [
      0x4D, 0xDC, 0xD8, 0xF6, 0x0B, 0x98, 0xBE, 0x21,
      0x83, 0x0C, 0xEE, 0x5E, 0xF2, 0x27, 0x01, 0xF9,
      0x64, 0x1A, 0x44, 0x18, 0xD0, 0x4C, 0x04, 0x14,
      0xAE, 0xFF, 0x08, 0x87, 0x6B, 0x34, 0xAB, 0x56,
      0xA1, 0xD4, 0x25, 0xA1, 0x22, 0x58, 0x33, 0x54,
      0x9A, 0xDB, 0x84, 0x1B, 0x51, 0xC9, 0xB3, 0x17,
      0x6A, 0x27, 0x2B, 0xDE, 0xBB, 0xA1, 0xD0, 0x78,
      0x47, 0x8F, 0x62, 0xB3, 0x97, 0xF3, 0x3C, 0x8D]
    -- From cryptsetup.
    t3_pass    = strBytes ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ++
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    t3_psize   = 65
    t3_salt    = strBytes "pass phrase exceeds block size"
    t3_ssize   = 30
    t3_c       = 1200
    t3_dk_len  = 32
    t3_derived = [
      0x22, 0x34, 0x4B, 0xC4, 0xB6, 0xE3, 0x26, 0x75,
      0xA8, 0x09, 0x0F, 0x3E, 0xA8, 0x0B, 0xE0, 0x1D,
      0x5F, 0x95, 0x12, 0x6A, 0x2C, 0xDD, 0xC3, 0xFA,
      0xCC, 0x4A, 0x5E, 0x6D, 0xCA, 0x04, 0xEC, 0x58]
  in
    testsuite "PBKDF2xHMACxSHA256" [
      test "RFC7914x00001" [
        expect_memeq "t1_derived" t1_derived $
          KDF.pbkdf2_hmac_sha256 t1_pass t1_psize t1_salt t1_ssize t1_c t1_dk_len],
      test "RFC7914x80000" [
        expect_memeq "t2_derived" t2_derived $
          KDF.pbkdf2_hmac_sha256 t2_pass t2_psize t2_salt t2_ssize t2_c t2_dk_len],
      test "Cryptsetup01200" [
        expect_memeq "t3_derived" t3_derived $
          KDF.pbkdf2_hmac_sha256 t3_pass t3_psize t3_salt t3_ssize t3_c t3_dk_len]]
