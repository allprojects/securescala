/**
 * AES Encryption Scheme (DETERMINISTIC).
 *
 * This version of AES is using ECB mode which takes no IV. This makes this
 * scheme DETERMINISTIC. It is of course much less secure than the random
 * version of AES.
 *
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @author Julian Stephen <stephe22@purdue.edu>
 *
 * @date 19/09/2014
 */

package crypsis;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.util.ArrayList;
import java.util.List;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import crypsis.Constants;
import crypsis.CrypsisException;

import org.apache.commons.codec.binary.Base64;

public class AES extends HomomorphicScheme {

	// TODO This is susceptible to prefix attack.
	private static final String ALGORITHM = "AES/ECB/PKCS5Padding";

	// the length of the key in bits
	private static final int KEY_BITLENGTH = Constants.KEY_BITLENGTH.get("AES");

	private static final String DELIMITER = " ";

	private Cipher cipherEncrypt = null;
	private Cipher cipherDecrypt = null;

	private final boolean isSentence;

	/**
	 * Takes a single key since AES is a symmetric encryption scheme.
	 *
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	public AES(String privateKeyPath, boolean sentence) {
		super(privateKeyPath);

		this.plaintextType = DataType.STRING;
		this.ciphertextType = DataType.STRING;

		// initialize encryption and decryption ciphers
		try {
			this.cipherEncrypt = Cipher.getInstance(ALGORITHM, "SunJCE");
			this.cipherEncrypt.init(Cipher.ENCRYPT_MODE,
					(SecretKey) this.privateKey);

			this.cipherDecrypt = Cipher.getInstance(ALGORITHM, "SunJCE");
			this.cipherDecrypt.init(Cipher.DECRYPT_MODE,
					(SecretKey) this.privateKey);
		} catch (NoSuchAlgorithmException | NoSuchProviderException
				| NoSuchPaddingException | InvalidKeyException e) {
			throw new CrypsisException("Unable to initialize AES cipher");
		}

		this.isSentence = sentence;

	}

	@Override
	protected void generateKeys() {
		KeyGenerator kgen = null;

		try {
			kgen = KeyGenerator.getInstance("AES");
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}

		// set the size of the key.
		kgen.init(KEY_BITLENGTH);

		SecretKey skeySpec = kgen.generateKey();

		// save key to file
		this.persistPrivateKey(skeySpec);
	}

	@Override
	public String encryptStr(String plaintext) {

		if (this.isSentence)
			return this.encryptSentense(plaintext, DELIMITER, false);

		if (plaintext == null)
			throw new CrypsisException("Empty plaintext in AES encryption");

		byte[] ciphertext = null;

		try {
			ciphertext = this.cipherEncrypt.doFinal(plaintext.getBytes());
		} catch (IllegalBlockSizeException | BadPaddingException e) {
			e.printStackTrace();
		}

		return Base64.encodeBase64String(ciphertext).trim();
	}

	/**
	 *
	 * @param delimiter
	 *            string based on which the sentence will be split
	 * @param compact
	 *            if true remove duplicated words
	 *
	 * @return
	 */
	public String encryptSentense(String sentence, String delimiter,
			boolean compact) {

		String[] parts = sentence.split(delimiter);
		List<String> seenBefore = new ArrayList<String>();

		String ciphertext = "";
		for (String part : parts) {

			if (part.equals("") || compact && seenBefore.contains(part))
				continue;

			seenBefore.add(part);

			byte[] partCiphertext = null;
			try {
				// encrypt current part
				partCiphertext = this.cipherEncrypt.doFinal(part.getBytes());

			} catch (IllegalBlockSizeException | BadPaddingException e) {
				e.printStackTrace();
			}

			ciphertext += Base64.encodeBase64String(partCiphertext).trim();

		}

		return ciphertext;
	}

	@Override
	public String decryptStr(String ciphertext) {

		if (ciphertext == null)
			throw new CrypsisException("Empty cipher text for AES decryption");

		byte[] plaintext = null;
		try {
			plaintext = this.cipherDecrypt.doFinal(Base64
					.decodeBase64(ciphertext));
		} catch (IllegalBlockSizeException | BadPaddingException e) {
			e.printStackTrace();
		}

		return new String(plaintext);
	}

	public static void main(String[] args) {
		HomomorphicScheme aes = HomomorphicScheme.getScheme("AES");
		String ctxt = (String) aes.encrypt("BRASS");
		System.out.println(ctxt);

		System.out.println(aes.decrypt(ctxt));

	}

}
