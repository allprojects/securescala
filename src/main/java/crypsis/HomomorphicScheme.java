package crypsis;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.util.Arrays;

import crypsis.Constants;
import crypsis.Constants.HomomorphicProperty;
import crypsis.CrypsisException;

/**
 * Partial Homomorphic Encryption Scheme abstract class. This class is used as
 * the parent for all schemes with a partial homomorphic encryption property,
 * including the RND (random) property.
 *
 * Please note: This class provides two constructor methods, one for symmetric
 * schemes and 1 for asymmetric schemes. The former needs a single parameter
 * (path to private key) and the latter needs two parameters (path to public
 * key, path to private key). The private key given can be null. A case this is
 * necessary is when the scheme instance is only used to perform the homomorphic
 * evaluation (requires public key only) and not to encrypt or decrypt. This is
 * often needed for the server side.
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @author Julian Stephen <stephe22@purdue.edu>
 *
 * @date 09/17/2014
 */
public abstract class HomomorphicScheme {
	// The extension to be given to files holding the public key.
	private static final String PUBLIC_EXTENSION = ".pk";

	// The extension to be given to files holding the private (secret) key.
	private static final String PRIVATE_EXTENSION = ".sk";

	// an encryption scheme can be either symmetric, which means it uses a
	// single key or asymmetric (public-key) which means it uses two keys, the
	// public one and the private one.
	private static final byte SYMMETRIC_ENCRYPTION = 0;
	private static final byte ASYMMETRIC_ENCRYPTION = 1;

	final String publicKeyFilePath;
	final String privateKeyFilePath;

	final Object publicKey;
	final Object privateKey;

	// Only for evaluation purposes: If true a random value is generated when
	// encrypting, otherwise a fixed number is used
	public boolean enableRandom = true;

	// if the flag above is set to false, use this value instead of calculating
	// a new random every time. This is only used for evaluation purposes.
	BigInteger precomputedRandom;
	BigInteger precomputedRandRased;

	// holds the type of the encryption (symmetric/asymmetric)
	private final byte encryption_type;

	public enum DataType {
		STRING, BIGINTEGER
	}

	// the type of the input data expected by the scheme
	public DataType plaintextType;
	// the type of the output data of scheme
	public DataType ciphertextType;

	/**
	 * Constructor method for symmetric encryption schemes. Requires a single
	 * private key.
	 *
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	public HomomorphicScheme(String privateKeyPath) {

		// Let's set the type of the encryption to symmetric.
		this.encryption_type = HomomorphicScheme.SYMMETRIC_ENCRYPTION;

		// this is a symmetric encryption so we only need to set the private key
		// path.
		this.publicKeyFilePath = null;
		this.privateKeyFilePath = privateKeyPath;

		// Without the private key we cannot do anything.
		if (this.privateKeyFilePath == null)
			throw new CrypsisException("Private key path cannot be null in "
					+ "symmetric schemes");

		// if key already exists, use it and don't create new one. Otherwise
		// generate new keys and save them in files.
		if (!this.keysExist())
			this.generateKeys();

		// read the keys from files.
		this.publicKey = null;
		this.privateKey = this.readPrivateKey();
	}

	/**
	 * Constructor method for asymmetric encryption schemes. Requires two keys.
	 * The private key can be null. This is useful in cases the scheme is only
	 * used to evaluate a value (e.g perform homomorphic addition) in which case
	 * the private key is not needed and probably not available.
	 *
	 * @param publicKeyPath
	 *            the path to the file holding the public key.
	 * @param privateKeyPath
	 *            the path to the file holding the private key.
	 */
	HomomorphicScheme(String publicKeyPath, String privateKeyPath) {

		// Let's set the type of the encryption to asymmetric.
		this.encryption_type = HomomorphicScheme.ASYMMETRIC_ENCRYPTION;

		// set the public key file path
		this.publicKeyFilePath = publicKeyPath;
		this.privateKeyFilePath = privateKeyPath;

		// without the public key we cannot do anything.
		if (this.publicKeyFilePath == null)
			throw new CrypsisException("Public key path cannot be null");

		// if only public key path is given then the public key must exist, it
		// cannot be created since a public key without a private key is
		// useless.
		if (this.privateKeyFilePath == null && !this.keysExist())
			throw new CrypsisException("Could not find public key");

		// if keys already exist, use them and don't create new ones. Otherwise
		// generate new keys and save them in files.
		if (this.privateKeyFilePath != null && !this.keysExist())
			this.generateKeys();

		// read the keys from files.
		this.publicKey = this.readPublicKey();

		// sometimes a private key might not be provided, i.e in the server side
		// when only the evaluate function is needed.
		if (this.privateKeyFilePath != null)
			this.privateKey = this.readPrivateKey();
		else
			this.privateKey = null;
	}

	/**
	 * This method is used to check whether the required keys for the encryption
	 * scheme exist of not.
	 *
	 * @return true if the required keys exist and false otherwise
	 */
	private boolean keysExist() {
		// the encryption scheme only needs a public key if it is of asymmetric
		// type.
		if (this.encryption_type == HomomorphicScheme.ASYMMETRIC_ENCRYPTION) {
			File publicKey = new File(this.publicKeyFilePath);
			if (!publicKey.exists())
				return false;
		}

		// both symmetric and asymmetric schemes have a private key but it can
		// be null
		if (this.privateKeyFilePath != null) {
			File privateKey = new File(this.privateKeyFilePath);
			if (!privateKey.exists())
				return false;
		}

		return true;
	}

	/**
	 * Given an object and a file path, generate a file and store that object.
	 *
	 * @param obj
	 *            the object to save
	 * @param filePath
	 *            the file path of the file in which the object will be saved.
	 */
	private void saveObjectToFile(Object obj, String filePath) {
		// Save secret key to files.
		ObjectOutputStream keyOS = null;
		try {
			File file = new File(filePath);

			// if parent directories don't exist, create them.
			if (file.getParentFile() != null) {
				file.getParentFile().mkdirs();
			}

			// create a new file and write the object to it.
			file.createNewFile();
			keyOS = new ObjectOutputStream(new FileOutputStream(file));
			keyOS.writeObject(obj);

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				keyOS.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Given an object representing the private key for this encryption scheme,
	 * save it to a file as indicated by the private key file path field.
	 *
	 * @param privateKey
	 *            the private key to store in a file.
	 */
	protected void persistPrivateKey(Object privateKey) {
		// Save private key to file.
		this.saveObjectToFile(privateKey, this.privateKeyFilePath);
	}

	/**
	 * Given two objects representing the public and private keys for this
	 * encryption scheme, save them to files as indicated by the public and
	 * private key file path fields.
	 *
	 * @param publicKey
	 *            the public key to store in a file
	 * @param privateKey
	 *            the private key to store in a file.
	 *
	 * @exception CrypsisException
	 *                if this scheme is not of asymmetric encryption type.
	 */
	protected void persistKeys(Object publicKey, Object privateKey) {
		// this method can only be used for asymmetric schemes.
		if (this.encryption_type != HomomorphicScheme.ASYMMETRIC_ENCRYPTION)
			throw new CrypsisException("This method can only be called for "
					+ "asymmetric encryption schemes.");

		// Save public key to file.
		this.saveObjectToFile(publicKey, this.publicKeyFilePath);

		// Save private key to file.
		this.persistPrivateKey(privateKey);
	}

	/**
	 * Some encryption schemes make use of an initial vector to add randomness.
	 * Given an object representing an iv, this method saves it to a file.
	 *
	 * @param iv
	 *            the initial vector to save to file.
	 */
	protected void persistIV(Object iv) {
		// Save iv to file.
		this.saveObjectToFile(iv, this.privateKeyFilePath + ".iv");
	}

	/**
	 * Given a file path, returns the object stored in that file.
	 *
	 * @param filePath
	 *            the path to the file from which a single object will be read.
	 *
	 * @return the object read from the file path given
	 */
	private Object readObjectFromFile(String filePath) {
		ObjectInputStream inputStream = null;
		Object obj = null;

		try {
			inputStream = new ObjectInputStream(new FileInputStream(filePath));
			obj = inputStream.readObject();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (inputStream != null)
					inputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return obj;
	}

	/**
	 *
	 * @return the public key read from the public key file.
	 */
	private Object readPublicKey() {
		return this.readObjectFromFile(this.publicKeyFilePath);
	}

	/**
	 *
	 * @return the private key read from the private key file.
	 */
	private Object readPrivateKey() {
		return this.readObjectFromFile(this.privateKeyFilePath);
	}

	/**
	 *
	 * @return the initial vector read from the initial vector file.
	 */
	protected Object readIV() {
		return this.readObjectFromFile(this.privateKeyFilePath + ".iv");
	}

	/**
	 * Generate required Keys. Both public and private keys for asymmetric
	 * schemes, only private key for symmetric schemes
	 */
	protected abstract void generateKeys();

	protected Object encryptStr(String plaintext) {
		throw new CrypsisException("Scheme does not support encrypt string");
	}

	protected Object encryptBI(BigInteger plaintext) {
		throw new CrypsisException(
				"Scheme does not support encrypt big integer");
	}

	public Object encrypt(Object plaintext) {

		if (this.plaintextType == DataType.STRING)
			return this.encryptStr(HomomorphicScheme.toString(plaintext));

		if (this.plaintextType == DataType.BIGINTEGER)
			return this.encryptBI(HomomorphicScheme.toBigInteger(plaintext));

		throw new CrypsisException("Unexpected plaintext type");
	}

	protected Object decryptStr(String plaintext) {
		throw new CrypsisException("Scheme does not support decrypt string");
	}

	protected Object decryptBI(BigInteger plaintext) {
		throw new CrypsisException(
				"Scheme does not support decrypt big integer");
	}

	public Object decrypt(Object ciphertext) {

		if (this.ciphertextType == DataType.STRING)
			return this.decryptStr(HomomorphicScheme.toString(ciphertext));

		if (this.ciphertextType == DataType.BIGINTEGER)
			return this.decryptBI(HomomorphicScheme.toBigInteger(ciphertext));

		throw new CrypsisException("Unexpected ciphertext type");
	}

	protected Object evaluateStr(String ciphertext, String ciphertext2) {
		throw new CrypsisException("Scheme does not evaluate decrypt string");
	}

	protected Object evaluateBI(BigInteger ciphertext, BigInteger ciphertext2) {
		throw new CrypsisException(
				"Scheme does not support evaluate big integer");
	}

	public Object evaluate(Object ciphertext, Object ciphertext2) {
		if (this.ciphertextType == DataType.STRING)
			return this.evaluateStr(HomomorphicScheme.toString(ciphertext),
					HomomorphicScheme.toString(ciphertext2));

		if (this.ciphertextType == DataType.BIGINTEGER)
			return this.evaluateBI(HomomorphicScheme.toBigInteger(ciphertext),
					HomomorphicScheme.toBigInteger(ciphertext2));

		throw new CrypsisException("Scheme does not support evaluate");
	}

	public static Object reEncrypt(HomomorphicScheme from,
			HomomorphicScheme to, Object ciphertext) {
		return to.encrypt(from.decrypt(ciphertext));
	}

	public static String toString(Object plaintext) {

		if (plaintext instanceof Integer)
			return ((Integer) plaintext).toString();

		if (plaintext instanceof Long)
			return ((Long) plaintext).toString();

		if (plaintext instanceof Double)
			return ((Double) plaintext).toString();

		if (plaintext instanceof String)
			return (String) plaintext;

		if (plaintext instanceof BigInteger)
			return ((BigInteger) plaintext).toString();

		throw new CrypsisException("Unexpected type of plaintext");
	}

	public static BigInteger toBigInteger(Object plaintext) {

		if (plaintext instanceof Integer)
			return new BigInteger("" + plaintext);

		if (plaintext instanceof Long)
			return new BigInteger("" + plaintext);

		if (plaintext instanceof Double)
			return new BigInteger(""
					+ (long) ((Double) plaintext).doubleValue());

		if (plaintext instanceof String)
			return new BigInteger((String) plaintext);

		if (plaintext instanceof BigInteger)
			return (BigInteger) plaintext;

		throw new CrypsisException("Unexpected type of plaintext: "
				+ plaintext.getClass().getName());
	}

	/**
	 * This method is used to return a partial homomorphic encryption scheme
	 * according to the phe property required. This allows for a higher level
	 * interface where we don't have to deal with individual encryption schemes
	 * but rather work with homomorphic properties. Consequently, an encryption
	 * scheme can be easily replaced by another scheme having the same
	 * homomorphic property, without having to change the design of the system.
	 *
	 * To make the design of the system more flexible, this method uses
	 * reflection so that if new schemes are introduced or old ones replaced,
	 * there will be no need to make edits here.
	 *
	 * @param property
	 *            the partial homomorphic property required
	 * @param keyId
	 *            the id to separate two schemes of the same property. For
	 *            example if two schemes of the same property are needed, but
	 *            each requires a different set of keys, then using different
	 *            key id will offer exactly this.
	 *
	 * @return an encryption scheme satisfying the required property.
	 */
	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			String path, int keyId, boolean sentence, int fullLength) {

		if (property == HomomorphicProperty.NONE)
			return null;

		// get the name of the scheme used for this partial homomorphic
		// encryption property.
		String schemeName = Constants.PROPERTY_TO_SCHEME.get(property);

		return getScheme(schemeName, path, keyId, sentence, fullLength);
	}

	/**
	 * This method is used to return a partial homomorphic encryption scheme
	 * according to the phe property required. This allows for a higher level
	 * interface where we don't have to deal with individual encryption schemes
	 * but rather work with homomorphic properties. Consequently, an encryption
	 * scheme can be easily replaced by another scheme having the same
	 * homomorphic property, without having to change the design of the system.
	 *
	 * To make the design of the system more flexible, this method uses
	 * reflection so that if new schemes are introduced or old ones replaced,
	 * there will be no need to make edits here.
	 *
	 * @param property
	 *            the partial homomorphic property required
	 * @param keyId
	 *            the id to separate two schemes of the same property. For
	 *            example if two schemes of the same property are needed, but
	 *            each requires a different set of keys, then using different
	 *            key id will offer exactly this.
	 *
	 * @return an encryption scheme satisfying the required property.
	 */
	public static HomomorphicScheme getScheme(String schemeName, String path,
			int keyId, boolean sentence, int fullLength) {

		// construct the key path indicating where the key files will be saved.
		String keyPath = path + schemeName + keyId;
		String publicKeyPath = keyPath + PUBLIC_EXTENSION;
		String privateKeyPath = keyPath + PRIVATE_EXTENSION;

		HomomorphicScheme homomorphicScheme = null;

		switch (schemeName) {

		case "OPE":
			homomorphicScheme = new OPE(privateKeyPath);
			break;

		case "OPESTR":
			if (fullLength == 0)
				throw new CrypsisException("Full Length for OPESTR not defined");

			homomorphicScheme = new OPESTR(privateKeyPath, fullLength);
			break;

		default:
			throw new CrypsisException("Unsupported scheme: " + schemeName);
		}

		return homomorphicScheme;
	}

	/**
	 * This method is used to return a partial homomorphic encryption scheme
	 * according to the phe property required. This allows for a higher level
	 * interface where we don't have to deal with individual encryption schemes
	 * but rather work with homomorphic properties. Consequently, an encryption
	 * scheme can be easily replaced by another scheme having the same
	 * homomorphic property, without having to change the design of the system.
	 *
	 * To make the design of the system more flexible, this method uses
	 * reflection so that if new schemes are introduced or old ones replaced,
	 * there will be no need to make edits here.
	 *
	 * @param property
	 *            the partial homomorphic property required
	 * @param keyId
	 *            the id to separate two schemes of the same property. For
	 *            example if two schemes of the same property are needed, but
	 *            each requires a different set of keys, then using different
	 *            key id will offer exactly this.
	 *
	 * @return an encryption scheme satisfying the required property.
	 */
	public static HomomorphicScheme getSchemeReflection(String schemeName,
			String path, int keyId) {

		// construct the key path indicating where the key files will be saved.
		String keyPath = path + schemeName + keyId;
		String publicKeyPath = keyPath + PUBLIC_EXTENSION;
		String privateKeyPath = keyPath + PRIVATE_EXTENSION;

		// to reflectively get a class we need the full name of the class. That
		// is package + name of class.
		String packageName = HomomorphicScheme.class.getPackage().getName();

		// get the class of this scheme.
		Class<?> schemeClass = null;
		try {
			schemeClass = Class.forName(packageName + "." + schemeName);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}

		// check if the scheme class is indeed a subclass of PHEScheme.
		if (!HomomorphicScheme.class.isAssignableFrom(schemeClass))
			throw new CrypsisException("Invalid scheme class");

		// we now initialize the phe scheme. If the scheme is symmetric the
		// constructor will only have 1 string parameter, representing the path
		// to the private key. Otherwise if it is an asymmetric scheme, it will
		// take two string parameters.
		HomomorphicScheme homomorphicScheme = null;

		// get the constructor of the scheme. If the scheme is symmetric the
		// constructor method will have one string argument. If asymmetric it
		// will have two string arguments.
		Constructor<?> constructor = null;
		boolean schemeIsSymmetric = true;
		try {
			// check whether the scheme is symmetric
			Class<?>[] types = { String.class };
			constructor = schemeClass.getConstructor(types);
			schemeIsSymmetric = true;
		} catch (NoSuchMethodException e) {
			try {
				// check whether the scheme is asymmetric
				Class<?>[] types = { String.class, String.class };
				constructor = schemeClass.getConstructor(types);
				schemeIsSymmetric = false;
			} catch (NoSuchMethodException ex) {
				// if it's neither then we have a problem.
				e.printStackTrace();
			}
		}

		if (constructor == null)
			throw new CrypsisException("Scheme constructor method not found.");

		// construct parameters according to whether the scheme is symmetric or
		// asymmetric.
		Object[] parameters = null;
		if (schemeIsSymmetric)
			parameters = new Object[] { privateKeyPath };
		else
			parameters = new Object[] { publicKeyPath, privateKeyPath };

		// and finally get the scheme instance.
		try {
			homomorphicScheme = (HomomorphicScheme) constructor
					.newInstance(parameters);
		} catch (InstantiationException | IllegalAccessException
				| IllegalArgumentException | InvocationTargetException e) {
			e.printStackTrace();

			StringWriter errors = new StringWriter();
			e.printStackTrace(new PrintWriter(errors));
			throw new CrypsisException(errors.toString());
		}

		if (homomorphicScheme == null)
			throw new CrypsisException("Homomorphic Scheme could not be "
					+ "returned for arguments: [schemeName=" + schemeName
					+ ", path=" + path + ", keyId=" + keyId + ", packageName="
					+ packageName + ", schemeClass" + schemeClass
					+ ", parameters=" + Arrays.toString(parameters) + "]");

		return homomorphicScheme;
	}

	/**
	 * if path for keys not explicitly given, use default key path and if key id
	 * not given use default key id.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(HomomorphicProperty property) {
		return getScheme(property, Constants.KEYS_PATH,
				Constants.DEFAULT_KEY_ID, false, 0);
	}

	/**
	 * if path for keys not explicitly given, use default key path.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			int keyId) {
		return getScheme(property, Constants.KEYS_PATH, keyId, false, 0);
	}

	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			boolean sentence) {
		return getScheme(property, Constants.KEYS_PATH,
				Constants.DEFAULT_KEY_ID, sentence, 0);
	}

	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			int keyId, int fullLength) {
		return getScheme(property, Constants.KEYS_PATH, keyId, false,
				fullLength);
	}

	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			int keyId, boolean sentence, int fullLength) {
		return getScheme(property, Constants.KEYS_PATH, keyId, sentence,
				fullLength);
	}

	/**
	 * if key id not given use default key id.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(HomomorphicProperty property,
			String path) {
		return getScheme(property, path, Constants.DEFAULT_KEY_ID, false, 0);
	}

	/**
	 * if path for keys not explicitly given, use default key path.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(String schemeName, int keyId) {
		return getScheme(schemeName, Constants.KEYS_PATH, keyId, false, 0);
	}

	public static HomomorphicScheme getScheme(String schemeName,
			boolean sentence) {
		return getScheme(schemeName, Constants.KEYS_PATH,
				Constants.DEFAULT_KEY_ID, sentence, 0);
	}

	public static HomomorphicScheme getScheme(String schemeName, int keyId,
			boolean sentence) {
		return getScheme(schemeName, Constants.KEYS_PATH, keyId, sentence, 0);
	}

	/**
	 * if path for keys not explicitly given, use default key path.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(String schemeName, int keyId,
			int fullLength) {
		return getScheme(schemeName, Constants.KEYS_PATH, keyId, false,
				fullLength);
	}

	/**
	 * if key id not given use default key id.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(String schemeName, String path) {
		return getScheme(schemeName, path, Constants.DEFAULT_KEY_ID, false, 0);
	}

	/**
	 * if path for keys not explicitly given, use default key path and if key id
	 * not given use default key id.
	 *
	 * @param property
	 * @return
	 */
	public static HomomorphicScheme getScheme(String schemeName) {
		return getScheme(schemeName, Constants.KEYS_PATH,
				Constants.DEFAULT_KEY_ID, false, 0);
	}

}
