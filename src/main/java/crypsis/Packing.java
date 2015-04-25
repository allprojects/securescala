/**
 * 
 * @author Savvas Savvides <savvas@purdue.edu>
 * @date 09/29/2014
 */
package crypsis;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import crypsis.Constants;
import crypsis.Constants.HomomorphicProperty;
import crypsis.CrypsisException;

public class Packing {

	// number of bits to use as padding between each number. This is independent
	// of the AHE scheme used. For N operands i.e N-1 addition operations with
	// all operands being of the same bit length, the bit overhead i.e
	// additional bits required to express the result is log2(N).
	final static int AHE_DEFAULT_PADDING_BITS = 3 * 8; // 3 bytes

	// used to calculate the default padding bits.
	final static int MHE_DEFAULT_ITEMS_PER_CTXT = 2;

	// the bit length of the largest number supported in our system
	final static int ITEM_BITS = Long.bitCount(Long.MAX_VALUE) + 1;

	// the homomorphic property we are packing for.
	private final HomomorphicProperty property;

	public Packing(HomomorphicProperty property) {
		this.property = property;
	}

	private int getPlaintextSpace() {

		// which scheme is used for this property? This is necessary since we
		// have multiple schemes of same property.
		String schemeName = Constants.PROPERTY_TO_SCHEME.get(this.property);

		if (!Constants.PLAINTEXT_SPACE.containsKey(schemeName))
			throw new CrypsisException("Unsupported scheme for packing: "
					+ schemeName);

		return Constants.PLAINTEXT_SPACE.get(schemeName);
	}

	/**
	 * Calculate the default padding bits according to homomorphic property
	 * used.
	 * 
	 * @return padding bits
	 */
	private int getDefaultPaddingBits() {

		int paddingBits = 0;
		if (property == HomomorphicProperty.AHE)
			paddingBits = AHE_DEFAULT_PADDING_BITS;
		else if (property == HomomorphicProperty.MHE) {

			// Y = P / (V + V*(N-1))
			// where:
			// P : plaintext space
			// V : item bits
			// N : # of records (items)
			// V*(N-1) : padding bits
			paddingBits = (int) Math.floor(getPlaintextSpace()
					/ (double) MHE_DEFAULT_ITEMS_PER_CTXT)
					- ITEM_BITS;
		} else
			throw new CrypsisException("Unsupported property for packing: "
					+ Constants.PROPERTY_TO_STRING.get(property));

		return paddingBits;
	}

	/**
	 * Concatenate two arrays
	 * 
	 * @param arr1
	 * @param arr2
	 * @return an array of length arr1.length + arr2.length containing all
	 *         elements of arr1 followed by all elements of arr2
	 */
	static byte[] concat(byte[] arr1, byte[] arr2) {
		byte[] r = new byte[arr1.length + arr2.length];

		System.arraycopy(arr1, 0, r, 0, arr1.length);
		System.arraycopy(arr2, 0, r, arr1.length, arr2.length);

		return r;
	}

	private byte[] packNumber(long number, byte[] packedNumber,
			int paddingBits, boolean left) {

		// initialize a zero byte array for padding.
		byte[] padding = new byte[paddingBits / 8];

		// get byte representation of number
		byte[] bytes = ByteBuffer.allocate(8).putLong(number).array();

		// add padding in front of number
		byte[] numberWithPadding = concat(padding, bytes);

		return left ? concat(numberWithPadding, packedNumber) : concat(
				packedNumber, numberWithPadding);
	}

	public byte[][] pack(long[] numbers) {

		// padding bits not explicitly given. Use default padding bits.
		int paddingBits = getDefaultPaddingBits();

		boolean mheConst = false;
		return pack(numbers, paddingBits, mheConst);
	}

	public byte[][] pack(long[] numbers, boolean mheConst) {

		// padding bits not explicitly given. Use default padding bits.
		int paddingBits = getDefaultPaddingBits();
		return pack(numbers, paddingBits, mheConst);
	}

	/**
	 * 
	 * @param property
	 * @param numbers
	 * @param paddingBits
	 * @return
	 */
	public byte[][] pack(long[] numbers, int paddingBits, boolean mheConst) {

		// get the bit length of the largest number that can fit in a single
		// plaintext.
		int plaintextSpace = getPlaintextSpace();

		// ciphertext should have space for at least 2 items otherwise there is
		// no point to use packing.
		if (2 * (ITEM_BITS + paddingBits) > plaintextSpace)
			throw new CrypsisException("Cannot pack items.");

		// empty byte array
		List<byte[]> packedPlaintextList = new ArrayList<byte[]>();

		// calculate how many items can fit in a single ciphertext according to
		// the length of each item and the padding chosen.
		int itemsPerCtxt = (int) Math.floor((double) plaintextSpace
				/ (ITEM_BITS + paddingBits));

		// Amendments that apply to MHE packing only and especially when we are
		// packing for a non constant multiplication
		if (this.property == HomomorphicProperty.MHE && !mheConst) {

			// In mhe constant case we allow multiple values to be packed but in
			// the non const case we allow exactly 2 values to be packed.
			if (itemsPerCtxt != 2)
				throw new CrypsisException("MHE packing supports only 2 items "
						+ "per ciphertext.");
		}

		// used to accumulate the packed items. Starts as empty array.
		byte[] packedNumber = new byte[0];

		// counts how many items were added in the current array so far.
		int counter = 0;

		// pack all numbers in reverse order.
		for (int i = 0; i < numbers.length; i++) {

			packedNumber = packNumber(numbers[i], packedNumber, paddingBits,
					false);
			counter++;

			if (packedNumber.length > plaintextSpace)
				throw new CrypsisException("Too long packed plaintext.");

			// if we packed enough items it's time to start over.
			if (counter == itemsPerCtxt) {

				// add the current packed plaintext.
				packedPlaintextList.add(packedNumber);

				// reset counter and accumulator variable.
				counter = 0;
				packedNumber = new byte[0];
			}
		}

		// if there are items not added yet add them.
		if (counter != 0) {

			// in this is a mhe packing, fill missing numbers with 1s.
			if (this.property == HomomorphicProperty.MHE && !mheConst)
				while (counter < itemsPerCtxt) {
					packedNumber = packNumber(1L, packedNumber, paddingBits,
							true);
					counter++;
				}

			packedPlaintextList.add(packedNumber);
		}

		byte[][] allPackedPlaintexts = packedPlaintextList
				.toArray(new byte[packedPlaintextList.size()][]);

		return allPackedPlaintexts;
	}

	public BigInteger[] unpack(byte[] packedByteArray) {

		// set padding bits to the default value according to homomorphic
		// property used.
		int paddingBits = getDefaultPaddingBits();

		boolean mheConst = false;
		return unpack(packedByteArray, paddingBits, mheConst);
	}

	public BigInteger[] unpack(byte[] packedByteArray, boolean mheConst) {

		// set padding bits to the default value according to homomorphic
		// property used.
		int paddingBits = getDefaultPaddingBits();
		return unpack(packedByteArray, paddingBits, mheConst);
	}

	/**
	 * 
	 * @param packedByteArray
	 * @return
	 */
	public BigInteger[] unpack(byte[] packedByteArray, int paddingBits,
			boolean mheConst) {

		// when unpacking, each item is made up of both the item bits and the
		// padding bits.
		int bytesPerItem = (ITEM_BITS + paddingBits) / 8;

		// Let's extract one such item at a time
		List<BigInteger> numbersArrayList = new ArrayList<BigInteger>();

		// trailing padding might have been removed so start unpacking from the
		// end.
		int index = packedByteArray.length;
		while (index > 0) {

			// calculate starting index of sub array.
			int fromIndex = 0;
			if (index - bytesPerItem > 0)
				fromIndex = index - bytesPerItem;

			// get the sub-array corresponding to the next packed number
			byte[] numberByteArray = Arrays.copyOfRange(packedByteArray,
					fromIndex, index);

			numbersArrayList.add(new BigInteger(numberByteArray));

			// move to the next number.
			index = fromIndex;
		}

		// if this is not a constant then:
		//
		// 10, 20
		// 3, 7 x
		// ---------
		// __ 70 140
		// 30 60 +
		// ---------
		// 30, 130, 140 (expected: 30, 140)
		if (this.property == HomomorphicProperty.MHE && !mheConst) {
			// keep the first and last values only.
			while (numbersArrayList.size() > 2)
				numbersArrayList.remove(1);
		}

		// convert to an array of numbers
		BigInteger[] numbers = new BigInteger[numbersArrayList.size()];
		for (int i = numbers.length - 1; i >= 0; i--)
			numbers[i] = numbersArrayList.get(numbers.length - i - 1);

		return numbers;
	}

}
