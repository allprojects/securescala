package crypsis;

import java.io.File;
import java.math.BigInteger;
import java.security.SecureRandom;

import crypsis.Constants;
import crypsis.CrypsisException;

/**
 * OPE Encryption Scheme (DETERMINISTIC).
 *
 * C++ implementation -- http://css.csail.mit.edu/cryptdb/
 *
 * Location of the native library:
 * -Djava.library.path=/.../.../PrivacyRepo/Crypsis/cryptdb_ope
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @date 11/10/2014
 */
public class OPE extends HomomorphicScheme {

	// load the native library. Do it statically so is only loaded once.
	static {
            String home = System.getProperty("user.home");
            System.load(home + "/libope.so");
	}

	// Size of plaintext and ciphertext: (a) ciphertext_bits cannot be less than
	// plaintext_bits (b) if plaintext_bits == ciphertext_bits then ciphertexts
	// are exactly the same as plaintexts since there is one-to-one
	// correspondence.
	private static final int PLAINTEXT_BITS = 64;
	private static final int CIPHERTEXT_BITS = 96;

	// the length of the key in bits
	private static final int KEY_BITLENGTH = Constants.KEY_BITLENGTH.get("OPE");

	// the limit imposed by the plaintext space. According to the number of bits
	// allowed in plaintext there is a limit (upper and lower) on the allowed
	// numbers.
	private static BigInteger LIMIT = new BigInteger("2").pow(
			OPE.PLAINTEXT_BITS).divide(new BigInteger("2"));

	private static BigInteger NEGATIVE_LIMIT = OPE.LIMIT.negate();
	private static BigInteger POSITIVE_LIMIT = OPE.LIMIT
			.subtract(BigInteger.ONE);

	// the secret key.
	private final String sk;

	/**
	 * Takes a single key since OPE is a symmetric encryption scheme.
	 *
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	public OPE(String privateKeyPath) {
		super(privateKeyPath);

		this.plaintextType = DataType.BIGINTEGER;
		this.ciphertextType = DataType.STRING;

		this.sk = (String) this.privateKey;

		// this condition is in place in case plaintext and ciphertext bits are
		// changed.
		int pb = OPE.PLAINTEXT_BITS; // this line to avoid warning.
		if (pb >= OPE.CIPHERTEXT_BITS)
			throw new CrypsisException("Plaintext bits should be less than "
					+ "ciphertext bits");
	}

	@Override
	protected void generateKeys() {

		// generate random string to use as a key.
		SecureRandom random = new SecureRandom();
		String skey = new BigInteger(KEY_BITLENGTH, random).toString(32);

		// save the key
		this.persistPrivateKey(skey);
	}

	/**
	 * Encrypt native interface.
	 *
	 * @param password
	 *            the password to be used in the encryption
	 * @param plaintext
	 *            the plaintext to be encrypted
	 * @return the resulting ciphertext
	 */
	public static native String encrypt(String password, String plaintext,
			int ptxtBits, int ctxtBits);

	/**
	 * Decrypt native interface.
	 *
	 * @param password
	 *            the password to be used for decryption
	 * @param ciphertext
	 *            the ciphertext to be decrypted
	 * @return the resulting plaintext
	 */
	public static native String decrypt(String password, String ciphertext,
			int ptxtBits, int ctxtBits);

	@Override
	public String encryptBI(BigInteger plaintextBI) {

		// is it less than the smallest value allowed?
		if (plaintextBI.compareTo(OPE.NEGATIVE_LIMIT) < 0)
			throw new CrypsisException("Plaintext " + plaintextBI.toString()
					+ " is less than the smallest value allowed "
					+ OPE.LIMIT.negate().toString());

		// is it greater than the largest value allowed?
		if (plaintextBI.compareTo(OPE.POSITIVE_LIMIT) > 0)
			throw new CrypsisException("Plaintext " + plaintextBI.toString()
					+ " is greater than the largest value allowed "
					+ OPE.LIMIT.subtract(BigInteger.ONE).toString());

		// OPE native implementation only accepts non-negative values. Let's
		// shift it to make negative numbers positive.
		plaintextBI = plaintextBI.add(OPE.LIMIT);

		// translate byte array to string.
		String plaintextStr = plaintextBI.toString();

		// encrypt using native interface.
		String ciphertextStr = this.encrypt(this.sk, plaintextStr,
				OPE.PLAINTEXT_BITS, OPE.CIPHERTEXT_BITS);

		return ciphertextStr;
	}

	@Override
	public BigInteger decryptStr(String ciphertext) {

		try {
			new BigInteger(ciphertext);
		} catch (Exception e) {
			throw new CrypsisException("Invalid OPE ciphertext: " + ciphertext);
		}

		// encrypt using native interface.
		String plaintextStr = this.decrypt(this.sk, ciphertext,
				OPE.PLAINTEXT_BITS, OPE.CIPHERTEXT_BITS);

		BigInteger plaintextBI = new BigInteger(plaintextStr);

		// reversed shifting to handle negative numbers.
		plaintextBI = plaintextBI.subtract(OPE.LIMIT);

		return plaintextBI;
	}

	public static void main(String[] args) {

		System.out.println("Supported range: " + OPE.NEGATIVE_LIMIT + " to "
				+ OPE.POSITIVE_LIMIT + "\n");

		HomomorphicScheme scheme = HomomorphicScheme.getScheme("OPE");

		BigInteger ptxt = new BigInteger("19980902");
		String ctxt = (String) scheme.encrypt(ptxt);
		System.out.println(ctxt);

		System.out.println(scheme.decrypt(ctxt));

	}
}
