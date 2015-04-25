/**
 * Holds the constants used throughout the system.
 *
 * includes:
 * - map that matches homomorphic properties to specific encryption scheme
 * - map that specifies the key length of each encryption scheme
 *
 * @author Savvas Savvides <savvas@purdue.edu>
 * @author Julian Stephen <stephe22@purdue.edu>
 *
 */

package crypsis;

import java.util.HashMap;
import java.util.Map;

public class Constants {

	// Partial Homomorphic Encryption Properties
	public enum HomomorphicProperty {
		NONE, RND, DET, AHE, MHE, DMHE, OPE, OPESTR, XOR
	}

	// match string to homomorphic property.
	public static final Map<String, HomomorphicProperty> STRING_TO_PROPERTY = new HashMap<String, HomomorphicProperty>();
	static {
		STRING_TO_PROPERTY.put("RND", HomomorphicProperty.RND);
		STRING_TO_PROPERTY.put("DET", HomomorphicProperty.DET);
		STRING_TO_PROPERTY.put("AHE", HomomorphicProperty.AHE);
		STRING_TO_PROPERTY.put("MHE", HomomorphicProperty.MHE);
		STRING_TO_PROPERTY.put("DMHE", HomomorphicProperty.DMHE);
		STRING_TO_PROPERTY.put("OPE", HomomorphicProperty.OPE);
		STRING_TO_PROPERTY.put("OPESTR", HomomorphicProperty.OPESTR);
		STRING_TO_PROPERTY.put("XOR", HomomorphicProperty.XOR);
	}

	public static final Map<HomomorphicProperty, String> PROPERTY_TO_STRING = new HashMap<HomomorphicProperty, String>();
	static {
		PROPERTY_TO_STRING.put(HomomorphicProperty.RND, "RND");
		PROPERTY_TO_STRING.put(HomomorphicProperty.DET, "DET");
		PROPERTY_TO_STRING.put(HomomorphicProperty.AHE, "AHE");
		PROPERTY_TO_STRING.put(HomomorphicProperty.MHE, "MHE");
		PROPERTY_TO_STRING.put(HomomorphicProperty.DMHE, "DMHE");
		PROPERTY_TO_STRING.put(HomomorphicProperty.OPE, "OPE");
		PROPERTY_TO_STRING.put(HomomorphicProperty.OPESTR, "OPESTR");
		PROPERTY_TO_STRING.put(HomomorphicProperty.XOR, "XOR");
	}

	// match Partial Homomorphic Properties to specific Encryption Schemes.
	public static final Map<HomomorphicProperty, String> PROPERTY_TO_SCHEME = new HashMap<HomomorphicProperty, String>();
	static {
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.RND, "AESRND");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.DET, "AES");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.AHE, "Paillier");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.MHE, "ElGamal");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.DMHE, "RSA");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.OPE, "OPE");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.OPESTR, "OPESTR");
		PROPERTY_TO_SCHEME.put(HomomorphicProperty.XOR, "GoldwasserMicali");
	}

	// match Partial Homomorphic Properties to specific Encryption Schemes.
	public static final Map<String, Integer> KEY_BITLENGTH = new HashMap<String, Integer>();
	static {
		KEY_BITLENGTH.put("AESRND", 0);
		KEY_BITLENGTH.put("AES", 128);
		KEY_BITLENGTH.put("Blowfish", 128);
		KEY_BITLENGTH.put("Paillier", 1024);
		KEY_BITLENGTH.put("ElGamal", 1024);
		KEY_BITLENGTH.put("RSA", 1024);
		KEY_BITLENGTH.put("OPE", 128);
		KEY_BITLENGTH.put("OPESTR", 128);
		KEY_BITLENGTH.put("GoldwasserMicali", 1024);
	}

	// match scheme to its plaintext space based on its key size. Plaintext
	// space indicates the largest integer number supported by the scheme.
	public static final Map<String, Integer> PLAINTEXT_SPACE = new HashMap<String, Integer>();
	static {
		PLAINTEXT_SPACE.put("Paillier", KEY_BITLENGTH.get("Paillier"));
		PLAINTEXT_SPACE.put("ElGamal", KEY_BITLENGTH.get("ElGamal") / 2);
	}

	public static final String CHARSET_NAME = "UTF-8";

	// the location where the keys will be stored or retrieved from. the full
	// key names are constructed as follows: KEYS_PATH + EncryptionScheme +
	// keyId + an appropriate extension indicating whether the key is the public
	// or the private one. Here, EncryptionScheme is the name of the encryption
	// algorithm and keyId is an integer identifying the specific encryption
	// keys.
	public static final String KEYS_PATH = "/tmp/";

	// If it's not necessary to use multiple keys, this default id will be used
	// for all the keys needed which means the same key pair will be used for
	// all encryptions of the same encryption scheme.
	public static final int DEFAULT_KEY_ID = 1;

	public static final int UDF_KEY_ID = 1;

	// serialization modes
	public enum SerializationMode {
		SERIALIZATION_KRYO, SERIALIZATION_JAVA
	}

	// the serialization mode to use.
	public static final SerializationMode SERIALIZATION_MODE = SerializationMode.SERIALIZATION_JAVA;

	public static final int MAXDATASIZE = 5120;
	public static final int REQQUESIZE = 4000;

	// whether to use the GMP library or not.
	public static boolean USE_GMP = false;

}
