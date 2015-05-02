package crypsis;

import java.math.BigInteger;
import java.security.SecureRandom;

import crypsis.Constants;
import crypsis.CrypsisException;

/**
 * OPESTR Encryption Scheme (DETERMINISTIC)
 *
 * Order preserving scheme applied on strings.
 *
 * C++ implementation -- http://css.csail.mit.edu/cryptdb/
 *
 * Location of the native library:
 * -Djava.library.path=/.../.../PrivacyRepo/Crypsis/cryptdb_ope
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @date 09 Mar 2015
 */
public class OPESTR extends HomomorphicScheme {
        // the length of the key in bits
	private static final int KEY_BITLENGTH = Constants.KEY_BITLENGTH
			.get("OPESTR");

	// the allowed characters in string OPESTR. Characters should appear in
	// ASCII increasing order.
	private static final String STRING_OPE_CHARSET = " 0123456789"
			+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	// base is the number of characters that can be encrypted under the string
	// OPE scheme. We add 1 to the allowed characters since the very first index
	// represents empty character. This is necessary so that we can compare
	// strings like "abcd" and "abc" in which the latter string is considered to
	// end with an empty character.
	private static final int CHARSET_BASE = OPESTR.STRING_OPE_CHARSET.length() + 1;
	private static final BigInteger CHARSET_BASE_BI = new BigInteger(""
			+ CHARSET_BASE);

	private static final int BITS_PER_CHAR = (int) Math.ceil(Math
			.log(CHARSET_BASE + 1) / Math.log(2));

	private static OPE OPE_SCHEME = (OPE) HomomorphicScheme.getScheme("OPE");

	// Size of plaintext and ciphertext: (a) ciphertext_bits cannot be less than
	// plaintext_bits (b) if plaintext_bits == ciphertext_bits then ciphertexts
	// are exactly the same as plaintexts since there is one-to-one
	// correspondence.
	private final int fullLength;
	private final int plaintextBits;
	private final int ciphertextBits;

	// the secret key.
	private final String sk;

	/**
	 * Takes a single key since OPE is a symmetric encryption scheme.
	 *
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	public OPESTR(String privateKeyPath, int fullLength) {
		super(privateKeyPath);

		this.plaintextType = DataType.STRING;
		this.ciphertextType = DataType.STRING;

		this.sk = (String) this.privateKey;

		this.fullLength = fullLength;
		// number of bits required to represent a string of this length.
		this.plaintextBits = fullLength * BITS_PER_CHAR;
		this.ciphertextBits = this.plaintextBits + 16;
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
	 *
	 * @param plaintext
	 * @param fullLength
	 * @return
	 */
	@Override
	public String encryptStr(String plaintext) {

		String plaintextNumeric = this.stringToNumericString(plaintext);

		return OPE_SCHEME.encrypt(this.sk, plaintextNumeric,
				this.plaintextBits, this.ciphertextBits);
	}

	/**
	 * Convert a string to big integer based on the char set allowed and the
	 * maximum length of strings expected
	 *
	 * @param plaintext
	 * @param fullLength
	 * @return
	 */
	private String stringToNumericString(String plaintext) {

		// remove null characters
		plaintext = plaintext.replaceAll("\0", "");

		// we only support caps to allow for longer strings. This can change by
		// including small caps in the charset.
		plaintext = plaintext.toUpperCase();

		if (plaintext.length() > this.fullLength)
			throw new CrypsisException("String too long for String OPE "
					+ "encryption");

		// the big integer that represents the string to encrypt
		BigInteger result = BigInteger.ZERO;

		for (int i = 0; i < plaintext.length(); i++) {

			int charIndex = OPESTR.STRING_OPE_CHARSET.indexOf(plaintext
					.charAt(i));

			// did we find the character?
			if (charIndex == -1)
				throw new CrypsisException("Character not allowed for String "
						+ "OPE encryption: " + plaintext.charAt(i));

			// add 1 because character 0 represents empty character.
			BigInteger coefficient = new BigInteger("" + (charIndex + 1));

			result = result.add(coefficient.multiply(CHARSET_BASE_BI
					.pow(this.fullLength - i - 1)));
		}

		return result.toString();
	}

	@Override
	public String decryptStr(String ciphertext) {

		if (ciphertext == null)
			throw new CrypsisException("Null ciphertext in opestr");

		try {
			new BigInteger(ciphertext);
		} catch (Exception e) {
			throw new CrypsisException("Invalid OPESTR ciphertext: "
					+ ciphertext + " of length: " + ciphertext.length());
		}

		String plaintext = OPE_SCHEME.decrypt((String) this.privateKey,
				ciphertext, this.plaintextBits, this.ciphertextBits);

		return this.numericStringToString(plaintext);
	}

	/**
	 * Given a byte array representing the plaintext, convert it to String.
	 *
	 * The byte array given as input parameter is expected to be of 2's
	 * complement representation.
	 *
	 * @param plaintextBA
	 *            a plaintext as a byte array
	 *
	 * @return plaintext as String.
	 */
	public String numericStringToString(String decrypted) {

		BigInteger plaintextBI = new BigInteger(decrypted);

		// lets find the greatest power contained in this plaintext. I won't use
		// logarithms here. to avoid importing other libraries.
		int power = 0;
		while (plaintextBI.divide(CHARSET_BASE_BI.pow(power + 1)).compareTo(
				BigInteger.ZERO) > 0)
			power++;

		String plaintextStr = "";
		for (int i = power; i >= 0; i--) {

			BigInteger basePow = CHARSET_BASE_BI.pow(i);

			int charIndex = plaintextBI.divide(basePow).intValue() - 1;

			if (charIndex == -1)
				break;

			plaintextStr += STRING_OPE_CHARSET.charAt(charIndex);

			plaintextBI = plaintextBI.mod(basePow);
		}

		return plaintextStr;
	}

	public static void main(String[] args) {

		System.out.println("Bits per char: " + BITS_PER_CHAR);

		int maxLength = 7;
		HomomorphicScheme scheme = HomomorphicScheme.getScheme("OPESTR", 1,
				maxLength);

		String ctxt = (String) scheme.encrypt("SHIP");
		System.out.println(ctxt);

		System.out.println(scheme.decrypt(ctxt));

	}

}
