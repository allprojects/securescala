/**
 * Paillier Encryption Scheme (AHE).
 *
 * Paillier is:
 * - Not Deterministic
 * - ADDITIVE homomorphic encryption scheme
 * - MULTIPLICATIVE if raised to the power of UNENCRYPTED value.
 *
 * http://en.wikipedia.org/wiki/Paillier_cryptosystem
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @author Julian Stephen <stephe22@purdue.edu>
 *
 * @date 09/17/2014
 */

package crypsis;

import java.io.Serializable;
import java.math.BigInteger;
import java.security.SecureRandom;

import crypsis.Constants;

public class Paillier extends HomomorphicScheme {

	// the bit length of public key. specifically of n.
	static final int KEY_BITLENGTH = Constants.KEY_BITLENGTH.get("Paillier");

	// whether to handle positive and negative numbers.
	private static final boolean NEGATIVE_NUMBERS_SUPPORT = true;

	// threshold that separates positive from negative numbers
	private static final BigInteger DECRYPTION_THRESHOLD = new BigInteger("2")
			.pow(KEY_BITLENGTH / 2);

	// the random number generator used in encrypting and in generating the
	// keys.
	static final SecureRandom RNG = new SecureRandom();

	// re-used values calculated only once in constructor.
	private PaillierPK pk;
	private BigInteger sk;
	private BigInteger u;

	/**
	 * Paillier is an asymmetric cryptosystem which means it requires two keys.
	 *
	 * @param publicKeyPath
	 *            the path to the file holding the public key.
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	public Paillier(String publicKeyPath, String privateKeyPath) {
		super(publicKeyPath, privateKeyPath);

		this.pk = (PaillierPK) this.publicKey;
		this.sk = (BigInteger) this.privateKey;

		this.plaintextType = DataType.BIGINTEGER;
		this.ciphertextType = DataType.BIGINTEGER;

		this.u = this.pk.g.modPow(this.sk, this.pk.nsquared)
				.subtract(BigInteger.ONE).divide(this.pk.n)
				.modInverse(this.pk.n);

		this.precomputedRandom = new BigInteger(KEY_BITLENGTH, RNG);
		this.precomputedRandRased = this.precomputedRandom.modPow(this.pk.n,
				this.pk.nsquared);
	}

	@Override
	protected void generateKeys() {

		// Construct two randomly generated positive BigIntegers that are
		// probably prime, with the specified bitLength and certainty.
		BigInteger p = BigInteger
				.probablePrime(KEY_BITLENGTH / 2, Paillier.RNG);
		BigInteger q = BigInteger
				.probablePrime(KEY_BITLENGTH / 2, Paillier.RNG);

		BigInteger n = p.multiply(q);
		BigInteger nsquared = n.multiply(n);

		BigInteger fn = p.subtract(BigInteger.ONE).multiply(
				q.subtract(BigInteger.ONE));

		// g: a random integer in Z*_{n^2} where gcd (L(g^lambda mod n^2), n) =
		// 1.
		BigInteger g = new BigInteger("2");

		// lambda = lcm(p-1, q-1) = (p-1)*(q-1)/gcd(p-1, q-1).
		BigInteger lambda = fn.divide(p.subtract(BigInteger.ONE).gcd(
				q.subtract(BigInteger.ONE)));

		// check whether g is good.
		if (g.modPow(lambda, nsquared).subtract(BigInteger.ONE).divide(n)
				.gcd(n).intValue() != 1) {
			System.out.println("g is not good. Choose g again.");
			System.exit(1);
		}

		PaillierPK publicKey = new PaillierPK(n, nsquared, g);
		this.persistKeys(publicKey, lambda);

		// pregenEncryptMultiplier();
	}

	@Override
	public BigInteger encryptBI(BigInteger plaintext) {

		BigInteger rand = this.precomputedRandom;
		BigInteger randRaised = this.precomputedRandRased;
		if (this.enableRandom) {
			rand = new BigInteger(KEY_BITLENGTH, Paillier.RNG);
			randRaised = rand.modPow(this.pk.n, this.pk.nsquared);
		}

		BigInteger gRaised = this.pk.g.modPow(plaintext, this.pk.nsquared);

		BigInteger ciphertext = gRaised.multiply(randRaised).mod(
				this.pk.nsquared);

		return ciphertext;
	}

	@Override
	public BigInteger decryptBI(BigInteger ciphertext) {

		// decrypt
		BigInteger plaintext = ciphertext.modPow(this.sk, this.pk.nsquared)
				.subtract(BigInteger.ONE).divide(this.pk.n).multiply(this.u)
				.mod(this.pk.n);

		// handle negative numbers if necessary.
		if (NEGATIVE_NUMBERS_SUPPORT
				&& plaintext.compareTo(DECRYPTION_THRESHOLD) >= 0)
			plaintext = plaintext.subtract(this.pk.n);

		return plaintext;
	}

	@Override
	public BigInteger evaluateBI(BigInteger ciphertextA, BigInteger ciphertextB) {
		return ciphertextA.multiply(ciphertextB).mod(this.pk.nsquared);
	}

	public BigInteger evaluatePlaintext(Object ciphertext, int plaintext) {
		return HomomorphicScheme.toBigInteger(ciphertext).modPow(
				new BigInteger("" + plaintext), this.pk.nsquared);
	}

	public BigInteger evaluateNegate(Object ciphertext) {
		return this.evaluatePlaintext(ciphertext, -1);
	}

	public BigInteger evaluateSubtract(Object ciphertext1, Object ciphertext2) {
		return this.evaluateBI(HomomorphicScheme.toBigInteger(ciphertext1),
				this.evaluateNegate(ciphertext2));
	}

	public static void testRandEncr() {
		HomomorphicScheme scheme = HomomorphicScheme.getScheme("Paillier");

		int COUNTER = 100000;
		scheme.enableRandom = true;
		long start = System.currentTimeMillis();
		for (int i = 0; i < COUNTER; i++)
			scheme.encrypt(i);
		System.out.println(System.currentTimeMillis() - start);

		scheme.enableRandom = false;
		start = System.currentTimeMillis();
		for (int i = 0; i < COUNTER; i++)
			scheme.encrypt(i);
		System.out.println(System.currentTimeMillis() - start);

	}

	public static void main(String[] args) {

		HomomorphicScheme scheme = HomomorphicScheme.getScheme("Paillier");

		scheme.enableRandom = false;
		BigInteger c = (BigInteger) scheme.encrypt(1);
		System.out.println(c);

	}
}

class PaillierPK implements Serializable {
	private static final long serialVersionUID = 3L;

	final BigInteger n;
	final BigInteger nsquared;
	final BigInteger g;

	PaillierPK(BigInteger n, BigInteger nsquared, BigInteger g) {
		this.n = n;
		this.nsquared = nsquared;
		this.g = g;
	}

}
