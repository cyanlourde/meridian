/*
 * OCaml C stubs for libsodium, loaded via dlopen at runtime.
 * No compile-time dependency on libsodium-dev.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <dlfcn.h>
#include <string.h>

/* Function pointer types */
typedef int (*sodium_init_fn)(void);
typedef int (*blake2b_fn)(unsigned char *out, size_t outlen,
    const unsigned char *in_, unsigned long long inlen,
    const unsigned char *key, size_t keylen);
typedef int (*ed25519_sign_fn)(unsigned char *sig,
    unsigned long long *siglen_p,
    const unsigned char *m, unsigned long long mlen,
    const unsigned char *sk);
typedef int (*ed25519_verify_fn)(const unsigned char *sig,
    const unsigned char *m, unsigned long long mlen,
    const unsigned char *pk);
typedef int (*ed25519_keypair_fn)(unsigned char *pk, unsigned char *sk);

/* Cached function pointers */
static void *sodium_lib = NULL;
static blake2b_fn fn_blake2b = NULL;
static ed25519_verify_fn fn_ed25519_verify = NULL;
static ed25519_sign_fn fn_ed25519_sign = NULL;
static ed25519_keypair_fn fn_ed25519_keypair = NULL;

static int load_sodium(void) {
    if (sodium_lib) return 1;
    sodium_lib = dlopen("libsodium.so.23", RTLD_LAZY);
    if (!sodium_lib) sodium_lib = dlopen("libsodium.so", RTLD_LAZY);
    if (!sodium_lib) return 0;

    sodium_init_fn init = (sodium_init_fn)dlsym(sodium_lib, "sodium_init");
    if (init) init();

    fn_blake2b = (blake2b_fn)dlsym(sodium_lib, "crypto_generichash_blake2b");
    fn_ed25519_verify = (ed25519_verify_fn)dlsym(sodium_lib,
        "crypto_sign_ed25519_verify_detached");
    fn_ed25519_sign = (ed25519_sign_fn)dlsym(sodium_lib,
        "crypto_sign_ed25519_detached");
    fn_ed25519_keypair = (ed25519_keypair_fn)dlsym(sodium_lib,
        "crypto_sign_ed25519_keypair");
    return 1;
}

/* meridian_sodium_init : unit -> bool */
CAMLprim value meridian_sodium_init(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(load_sodium()));
}

/* meridian_sodium_available : unit -> bool */
CAMLprim value meridian_sodium_available(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(sodium_lib != NULL));
}

/* meridian_blake2b : bytes -> int -> bytes */
CAMLprim value meridian_blake2b(value v_input, value v_outlen) {
    CAMLparam2(v_input, v_outlen);
    CAMLlocal1(v_output);

    if (!load_sodium() || !fn_blake2b)
        caml_failwith("libsodium not available");

    size_t outlen = Int_val(v_outlen);
    if (outlen < 1 || outlen > 64)
        caml_failwith("blake2b: output length must be 1..64");

    size_t inlen = caml_string_length(v_input);
    v_output = caml_alloc_string(outlen);

    int ret = fn_blake2b(
        (unsigned char *)Bytes_val(v_output), outlen,
        (const unsigned char *)Bytes_val(v_input), inlen,
        NULL, 0);

    if (ret != 0)
        caml_failwith("blake2b: hash failed");

    CAMLreturn(v_output);
}

/* meridian_ed25519_verify : bytes -> bytes -> bytes -> bool */
CAMLprim value meridian_ed25519_verify(value v_pk, value v_msg, value v_sig) {
    CAMLparam3(v_pk, v_msg, v_sig);

    if (!load_sodium() || !fn_ed25519_verify)
        caml_failwith("libsodium ed25519 not available");

    if (caml_string_length(v_pk) != 32)
        caml_failwith("ed25519_verify: public key must be 32 bytes");
    if (caml_string_length(v_sig) != 64)
        caml_failwith("ed25519_verify: signature must be 64 bytes");

    int ret = fn_ed25519_verify(
        (const unsigned char *)Bytes_val(v_sig),
        (const unsigned char *)Bytes_val(v_msg),
        caml_string_length(v_msg),
        (const unsigned char *)Bytes_val(v_pk));

    CAMLreturn(Val_bool(ret == 0));
}

/* meridian_ed25519_keypair : unit -> (bytes * bytes) */
CAMLprim value meridian_ed25519_keypair(value unit) {
    CAMLparam1(unit);
    CAMLlocal3(v_pk, v_sk, v_pair);

    if (!load_sodium() || !fn_ed25519_keypair)
        caml_failwith("libsodium ed25519 not available");

    v_pk = caml_alloc_string(32);
    v_sk = caml_alloc_string(64);

    fn_ed25519_keypair(
        (unsigned char *)Bytes_val(v_pk),
        (unsigned char *)Bytes_val(v_sk));

    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, v_pk);
    Store_field(v_pair, 1, v_sk);
    CAMLreturn(v_pair);
}

/* meridian_ed25519_sign : bytes -> bytes -> bytes */
CAMLprim value meridian_ed25519_sign(value v_msg, value v_sk) {
    CAMLparam2(v_msg, v_sk);
    CAMLlocal1(v_sig);

    if (!load_sodium() || !fn_ed25519_sign)
        caml_failwith("libsodium ed25519 not available");

    if (caml_string_length(v_sk) != 64)
        caml_failwith("ed25519_sign: secret key must be 64 bytes");

    v_sig = caml_alloc_string(64);
    unsigned long long siglen;

    fn_ed25519_sign(
        (unsigned char *)Bytes_val(v_sig), &siglen,
        (const unsigned char *)Bytes_val(v_msg),
        caml_string_length(v_msg),
        (const unsigned char *)Bytes_val(v_sk));

    CAMLreturn(v_sig);
}
