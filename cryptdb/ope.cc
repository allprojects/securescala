#include <jni.h>
#include <stdio.h>

#include "crypsis_OPE.h"

#include <NTL/ZZ.h>
#include <NTL/RR.h>

#include "ope.hh"
#include "prng.hh"
#include "hgd.hh"
#include "aes.hh"
#include "sha.hh"
#include "hmac.hh"
#include "zz.hh"

#include "urandom.hh"

using namespace std;
using namespace NTL;

JNIEXPORT jstring JNICALL Java_crypsis_OPE_encrypt(JNIEnv *env,
		jobject jobj, jstring passwordJStr, jstring plaintextJStr,
		jint ptxtBits, jint ctxtBits) {

	// translate from jstring to char*
	const char* plaintextStr = (*env).GetStringUTFChars(plaintextJStr, 0);
	const char* passwordChar = (*env).GetStringUTFChars(passwordJStr, 0);
	const string password(passwordChar);

	// translate from char* to ZZ
	ZZ plaintext = to_ZZ(plaintextStr);

	// release plaintext string to avoid memory leak.
	(*env).ReleaseStringUTFChars(plaintextJStr, plaintextStr);

	OPE o(password, ptxtBits, ctxtBits);

	// encrypt plaintext
	ZZ ciphertextZZ = o.encrypt(plaintext);

	// convert ZZ to char*
	std::stringstream buffer;
	buffer << ciphertextZZ;
	const char* ciphertextStr = buffer.str().c_str();

	// convert char* to jstring and return.
	return (*env).NewStringUTF(ciphertextStr);
}

JNIEXPORT jstring JNICALL Java_crypsis_OPE_decrypt(JNIEnv *env,
		jobject jobj, jstring passwordJStr, jstring ciphertextJStr,
		jint ptxtBits, jint ctxtBits) {

	// translate from jstring to char*
	const char *ciphertextStr = (*env).GetStringUTFChars(ciphertextJStr, 0);
	const char* passwordChar = (*env).GetStringUTFChars(passwordJStr, 0);
	const string password(passwordChar);

	// translate from char* to ZZ
	ZZ ciphertext = to_ZZ(ciphertextStr);

	// release plaintext string to avoid memory leak.
	(*env).ReleaseStringUTFChars(ciphertextJStr, ciphertextStr);

	int pbits = 32;
	int cbits = 64;

	OPE o(password, ptxtBits, ctxtBits);

	// decrypt plaintext
	ZZ plaintextZZ = o.decrypt(ciphertext);

	// convert ZZ to char*
	std::stringstream buffer;
	buffer << plaintextZZ;
	const char* plaintextStr = buffer.str().c_str();

	// convert char* to jstring and return.
	return (*env).NewStringUTF(plaintextStr);
}

/*
 * A gap is represented by the next integer value _above_ the gap.
 */
static ZZ domain_gap(const ZZ &ndomain, const ZZ &nrange, const ZZ &rgap,
		PRNG *prng) {
	return HGD(rgap, ndomain, nrange - ndomain, prng);
}

template<class CB>
ope_domain_range OPE::lazy_sample(const ZZ &d_lo, const ZZ &d_hi,
		const ZZ &r_lo, const ZZ &r_hi, CB go_low, blockrng<AES> *prng) {
	ZZ ndomain = d_hi - d_lo + 1;
	ZZ nrange = r_hi - r_lo + 1;
	throw_c(nrange >= ndomain);

	if (ndomain == 1)
		return ope_domain_range(d_lo, r_lo, r_hi);

	/*
	 * Deterministically reset the PRNG counter, regardless of
	 * whether we had to use it for HGD or not in previous round.
	 */
	auto v = hmac<sha256>::mac(
			StringFromZZ(d_lo) + "/" + StringFromZZ(d_hi) + "/" + StringFromZZ(
					r_lo) + "/" + StringFromZZ(r_hi), key);
	v.resize(AES::blocksize);
	prng->set_ctr(v);

	ZZ rgap = nrange / 2;
	ZZ dgap;

	auto ci = dgap_cache.find(r_lo + rgap);
	if (ci == dgap_cache.end()) {
		dgap = domain_gap(ndomain, nrange, nrange / 2, prng);
		dgap_cache[r_lo + rgap] = dgap;
	} else {
		dgap = ci->second;
	}

	if (go_low(d_lo + dgap, r_lo + rgap))
		return lazy_sample(d_lo, d_lo + dgap - 1, r_lo, r_lo + rgap - 1,
				go_low, prng);
	else
		return lazy_sample(d_lo + dgap, d_hi, r_lo + rgap, r_hi, go_low, prng);
}

template<class CB>
ope_domain_range OPE::search(CB go_low) {
	blockrng<AES> r(aesk);

	return lazy_sample(to_ZZ(0), to_ZZ(1) << pbits, to_ZZ(0),
			to_ZZ(1) << cbits, go_low, &r);
}

ZZ OPE::encrypt(const ZZ &ptext) {
	ope_domain_range dr =
	search([&ptext](const ZZ &d, const ZZ &) {return ptext < d;});

	auto v = sha256::hash(StringFromZZ(ptext));
	v.resize(16);

	blockrng<AES> aesrand(aesk);
	aesrand.set_ctr(v);

	ZZ nrange = dr.r_hi - dr.r_lo + 1;
	return dr.r_lo + aesrand.rand_zz_mod(nrange);
}

ZZ OPE::decrypt(const ZZ &ctext) {
	ope_domain_range dr =
	search([&ctext](const ZZ &, const ZZ &r) {return ctext < r;});
	return dr.d;
}

static void test_ope(int pbits, int cbits) {
	urandom u;
	OPE o("hello world", pbits, cbits);
	RR maxerr = to_RR(0);

	enum {
		niter = 100
	};

	for (uint i = 1; i < niter; i++) {
		ZZ pt = u.rand_zz_mod(to_ZZ(1) << pbits);
		ZZ ct = o.encrypt(pt);
		ZZ pt2 = o.decrypt(ct);
		throw_c(pt2 == pt);
		// cout << pt << " -> " << o.encrypt(pt, -1) << "/" << ct << "/" << o.encrypt(pt, 1) << " -> " << pt2 << endl;

		RR::SetPrecision(cbits + pbits);
		ZZ guess = ct / (to_ZZ(1) << (cbits - pbits));
		RR error = abs(to_RR(guess) / to_RR(pt) - 1);
		maxerr = max(error, maxerr);
		// cout << "pt guess is " << error << " off" << endl;
	}

	cout << "--- ope: " << pbits << "-bit plaintext, " << cbits
			<< "-bit ciphertext" << endl << "  enc/dec pair: " << " usec; "
			<< "~#bits leaked: "
			<< ((maxerr < pow(to_RR(2), to_RR(-pbits))) ? pbits : NumBits(
					to_ZZ(1 / maxerr))) << endl;
}

int main() {
	cout << "START" << endl << endl;

	int pbits = 32;
	int cbits = 128;
	OPE o("sadf67ONUy 4hofuc g", pbits, cbits);

	urandom u;

	ZZ pt = to_ZZ(5);
	// ZZ pt = to_ZZ(u.rand_zz_mod(to_ZZ(1) << pbits));
	ZZ ct = o.encrypt(pt);
	ZZ pt2 = o.decrypt(ct);

	cout << pt - 1 << " -> " << endl << o.encrypt(pt - 1) << endl;
	cout << pt << " -> " << endl << ct << " -> " << pt2 << endl;
	cout << pt + 1 << " -> " << endl << o.encrypt(pt + 1) << endl;

	/*
	 for (int pbits = 32; pbits <= 128; pbits += 32)
	 for (int cbits = pbits; cbits <= pbits + 128; cbits += 32)
	 test_ope(pbits, cbits);
	 */

	cout << "END" << endl;

}
