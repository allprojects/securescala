package crypsis;

import java.io.Serializable;
import java.math.BigInteger;
import java.security.SecureRandom;

import crypsis.Constants;
import crypsis.CrypsisException;

/**
 * ElGamal Encryption Scheme (MHE).
 *
 * ElGamal is: - Not Deterministic - MULTIPLICATIVELY homomorphic encryption
 * scheme - EXPONENTIAL if raised to the power of UNENCRYPTED value
 *
 * http://en.wikipedia.org/wiki/ElGamal_encryption
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @author Julian Stephen <stephe22@purdue.edu>
 *
 * @date 09/17/2014
 */
public class ElGamal extends HomomorphicScheme {

	// the length of the key in bits
	private static final int KEY_BITLENGTH = Constants.KEY_BITLENGTH
			.get("ElGamal");

	// whether to handle positive and negative numbers.
	private static final boolean NEGATIVE_NUMBERS_SUPPORT = true;

	// threshold that separates positive from negative numbers
	private static final BigInteger DECRYPTION_THRESHOLD = new BigInteger("2")
			.pow(KEY_BITLENGTH / 2);

	private static final String DELIMITER = ":";

	private SecureRandom RNG = new SecureRandom();

	// Only for evaluation purposes: If true a random value is generated when
	// encrypting, otherwise a fixed number is used
	static boolean ENABLE_RANDOM = true;

	// if the flag above is set to false, use this value instead of calculating
	// a new random every time. This is only used for evaluation purposes.
	private final BigInteger precomputedRandom;
	private final BigInteger precomputedC1;
	private final BigInteger precomputedS;

	ElGamalPK pk;
	BigInteger sk;

	public ElGamal(String pkPath, String skPath) {
		super(pkPath, skPath);

		this.plaintextType = DataType.BIGINTEGER;
		this.ciphertextType = DataType.STRING;

		this.pk = (ElGamalPK) this.publicKey;
		this.sk = (BigInteger) this.privateKey;

		this.precomputedRandom = new BigInteger(KEY_BITLENGTH, this.RNG);
		this.precomputedC1 = this.pk.g
				.modPow(this.precomputedRandom, this.pk.p);
		this.precomputedS = this.pk.h.modPow(this.precomputedRandom, this.pk.p);
	}

	@Override
	protected void generateKeys() {
		SecureRandom rng = new SecureRandom();

		// generate random p and g
		BigInteger p = BigInteger.probablePrime(KEY_BITLENGTH, rng);

		BigInteger g = BigInteger.probablePrime(KEY_BITLENGTH, rng);

		// generate private key
		BigInteger privateKey = null;
		while (true) {
			privateKey = BigInteger.probablePrime(KEY_BITLENGTH, rng);
			if (privateKey.gcd(p).equals(BigInteger.ONE))
				break;
		}

		// calculate h = g^private_key
		BigInteger h = g.modPow(privateKey, p);

		// save public and private keys
		this.persistKeys(new ElGamalPK(p, g, h), privateKey);
	}

	@Override
	public String encryptBI(BigInteger plaintext) {

		// check if plaintext is too big
		if (plaintext.bitLength() >= this.pk.p.bitLength() / 2)
			throw new CrypsisException("Plaintext too big for key bitlenght: "
					+ this.pk.p.bitLength());

		BigInteger rand = this.precomputedRandom;
		BigInteger c1 = this.precomputedC1;
		BigInteger s = this.precomputedS;
		if (ENABLE_RANDOM) {
			rand = new BigInteger(KEY_BITLENGTH, Paillier.RNG);

			// calculate first part of ciphertext
			c1 = this.pk.g.modPow(rand, this.pk.p);

			// calculate shared secret key
			s = this.pk.h.modPow(rand, this.pk.p);
		}

		// calculate second part of the ciphertext
		BigInteger c2 = (plaintext).multiply(s).mod(this.pk.p);

		return this.serialize(c1, c2);
	}

	@Override
	public BigInteger decryptStr(String ctxt) {

		BigInteger[] cParts = this.deserialize(ctxt);

		BigInteger inverse = cParts[0].modPow(this.sk, this.pk.p).modInverse(
				this.pk.p);

		BigInteger plaintext = cParts[1].multiply(inverse).mod(this.pk.p);

		if (NEGATIVE_NUMBERS_SUPPORT
				&& plaintext.compareTo(DECRYPTION_THRESHOLD) >= 0)
			plaintext = plaintext.subtract(this.pk.p);

		return plaintext;
	}

	@Override
	public String evaluateStr(String ciphertextA, String ciphertextB) {

		BigInteger[] cPartsA = this.deserialize(ciphertextA);
		BigInteger[] cPartsB = this.deserialize(ciphertextB);

		return cPartsA[0].multiply(cPartsB[0]) + ElGamal.DELIMITER
				+ cPartsA[1].multiply(cPartsB[1]);
	}

	public String evaluatePlaintext(String ciphertext, int pow) {

		ElGamalPK publicKey = (ElGamalPK) this.publicKey;

		BigInteger[] cParts = this.deserialize(ciphertext);

		return this.serialize(
				cParts[0].modPow(new BigInteger("" + pow), publicKey.p),
				cParts[0].modPow(new BigInteger("" + pow), publicKey.p));
	}

	private String serialize(BigInteger c1, BigInteger c2) {
		return c1.toString() + ElGamal.DELIMITER + c2.toString();
	}

	private BigInteger[] deserialize(String ctxt) {
		String[] cParts = ctxt.split(ElGamal.DELIMITER);

		if (cParts.length != 2)
			throw new CrypsisException("Invalid elgamal ciphertext A");

		BigInteger parts[] = new BigInteger[] { new BigInteger(cParts[0]),
				new BigInteger(cParts[1]) };
		return parts;
	}

	@SuppressWarnings("unused")
	private static void testRandEncr() {
		HomomorphicScheme scheme = HomomorphicScheme.getScheme("ElGamal");

		int COUNTER = 1000;
		ENABLE_RANDOM = true;
		long start = System.currentTimeMillis();
		for (int i = 0; i < COUNTER; i++)
			scheme.encrypt(i);
		System.out.println(System.currentTimeMillis() - start);

		ENABLE_RANDOM = false;
		start = System.currentTimeMillis();
		for (int i = 0; i < COUNTER; i++)
			scheme.encrypt(i);
		System.out.println(System.currentTimeMillis() - start);

	}

	public static void main(String[] args) {

		HomomorphicScheme scheme = HomomorphicScheme.getScheme("ElGamal");

		scheme.enableRandom = false;
		String c = (String) scheme.encrypt(6);
		String c2 = (String) scheme.encrypt(6);
		System.out.println(c);
		System.out.println(c2);
		String c3 = (String) scheme.evaluate(c, c2);
		System.out.println(scheme.decrypt(c3));
	}
}

/**
 * Used to enclose the values making up the ElGamal Public Key
 *
 */
class ElGamalPK implements Serializable {
	private static final long serialVersionUID = 1L;

	final BigInteger p;
	final BigInteger g;
	final BigInteger h;

	ElGamalPK(BigInteger p, BigInteger g, BigInteger h) {
		this.p = p;
		this.g = g;
		this.h = h;
	}

	@Override
	public String toString() {
		String repr = "p=" + this.p.toString() + "\n";
		repr += "g=" + this.g.toString() + "\n";
		repr += "h=" + this.h.toString() + "\n";

		return repr;
	}
}
