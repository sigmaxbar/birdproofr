import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import java.io.PrintWriter;

/**
 * Migration banding data proofer
 * 
 * @author Thea Sukianto, Heidi Ware
 *
 */
public class Proofer {
	// fix cap time
	private ArrayList<DataLine> data;
	private final String[] reqCategories = { "original order", "who entered", "master page #", "location", "bandsize",
			"disposition (band code)", "band #", "species", "age", "ha", "ha2", "sex", "hs", "hs2", "skull", "bp", "cp",
			"fat", "muscle", "b mlt", "ff mlt", "ff wear", "wing", "tail", "weight", "status", "date", "yyyy", "mm",
			"dd", "julian", "cap time", "site", "net #", "parasites?", "disposition", "notes", "proofing notes" };
	private int originalOrderIndex;
	private int whoEnteredIndex;
	private int masterPageNumberIndex;
	private int locationIndex;
	private int bandSizeIndex;
	private int dispositionIndex;
	private int bandNumberIndex;
	private int speciesIndex;
	private int ageIndex;
	private int howAgedIndex;
	private int howAged2Index;
	private int sexIndex;
	private int howSexedIndex;
	private int howSexed2Index;
	private int skullIndex;
	private int bpIndex;
	private int cpIndex;
	private int fatIndex;
	private int muscleIndex;
	private int bMltIndex;
	private int ffMltIndex;
	private int ffWearIndex;
	private int wingIndex;
	private int tailIndex;
	private int weightIndex;
	private int statusIndex;
	private int dateIndex;
	private int yearIndex;
	private int monthIndex;
	private int dayIndex;
	// different way to look at date
	private int julianIndex;
	private int capTimeIndex;
	private int siteIndex;
	private int netNumberIndex;
	private int parasitesIndex;
	private int dispIndex;
	private int notesIndex;
	private int proofingNotesIndex;
	private int counter;

	public static void main(String[] args) {
		new Proofer(new File("bad.csv"));
	}

	/**
	 * Constructor
	 * 
	 * @param path
	 */
	public Proofer(File file) {
		try {
			Scanner csvScanner = new Scanner(file);
			PrintWriter writer = new PrintWriter("proofed.csv");
			String[] fileCategories = csvScanner.nextLine().split(",");
			data = new ArrayList<DataLine>();
			for (String cat : reqCategories) {
				CategoryIndexValidatorAssigner(fileCategories, cat);
			}
			while (csvScanner.hasNextLine()) {
				String currentLine = csvScanner.nextLine();
				DataLine currentDataLine = new DataLine(currentLine);
				if (currentDataLine.getDataSize() == 0) {
					break;
				}
				// initialize all bird variables on the current line
				currentDataLine.setOrder(originalOrderIndex);
				currentDataLine.setWhoEntered(whoEnteredIndex);
				currentDataLine.setMasterPageNumber(masterPageNumberIndex);
				currentDataLine.setLocation(locationIndex);
				currentDataLine.setBandSize(bandSizeIndex);
				currentDataLine.setDisposition(dispositionIndex);
				currentDataLine.setBandNumber(bandNumberIndex);
				currentDataLine.setSpecies(speciesIndex);
				currentDataLine.setAge(ageIndex);
				currentDataLine.setHowAged(howAgedIndex);
				currentDataLine.setHowAged2(howAged2Index);
				currentDataLine.setSex(sexIndex);
				currentDataLine.setHowSexed(howSexedIndex);
				currentDataLine.setHowSexed2(howSexed2Index);
				currentDataLine.setSkull(skullIndex);
				currentDataLine.setBP(bpIndex);
				currentDataLine.setCP(cpIndex);
				currentDataLine.setFat(fatIndex);
				currentDataLine.setMuscle(muscleIndex);
				currentDataLine.setBMlt(bMltIndex);
				currentDataLine.setFFMlt(ffMltIndex);
				currentDataLine.setFFWear(ffWearIndex);
				currentDataLine.setWing(wingIndex);
				currentDataLine.setTail(tailIndex);
				currentDataLine.setWeight(weightIndex);
				currentDataLine.setStatus(statusIndex);
				currentDataLine.setDate(dateIndex);
				currentDataLine.setYear(yearIndex);
				currentDataLine.setMonth(monthIndex);
				currentDataLine.setDay(dayIndex);
				currentDataLine.setJulian(julianIndex);
				currentDataLine.setCapTime(capTimeIndex);
				currentDataLine.setSite(siteIndex);
				currentDataLine.setNetNum(netNumberIndex);
				currentDataLine.setParasites(parasitesIndex);
				currentDataLine.setDisp(dispIndex);
				currentDataLine.setNotes(notesIndex);
				currentDataLine.setProofingNotes(proofingNotesIndex);
				data.add(currentDataLine);
				if (currentDataLine.getSpecies().toLowerCase().equals("bade")
						|| currentDataLine.getSpecies().toLowerCase().equals("balo")) {
					proofBandSize(currentDataLine);
					proofDisposition(currentDataLine);
					writer.println(currentLine);
				} else {
					proofAll(currentDataLine);
					writer.println(currentLine);
				}
				if (currentDataLine.getProofingNotes().contains("*")) {
					currentLine = currentLine + currentDataLine.getProofingNotes() + " ";
					writer.println(currentLine);
				}

			}
			writer.close();
			csvScanner.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Assigns column numbers to categories
	 * 
	 * @param categories
	 *            list of valid categories
	 * @param category
	 *            category to be assigned
	 */
	private void CategoryIndexValidatorAssigner(String[] categories, String category) {

		// if (!Arrays.asList(categories).contains(category)) {
		// throw new IllegalArgumentException(category + " category not in data file");
		// }
		loop: for (int i = 0; i < categories.length; i++) {
			String currentCategory = categories[i].toLowerCase();
			if (currentCategory.equals(category)) {
				switch (category) {
				case "original order":
					originalOrderIndex = i;
					break loop;
				case "who entered":
					whoEnteredIndex = i;
					break loop;
				case "master page#":
					masterPageNumberIndex = i;
					break loop;
				case "location":
					locationIndex = i;
					break loop;
				case "bandsize":
					bandSizeIndex = i;
					break loop;
				case "disposition (band code)":
					dispositionIndex = i;
					break loop;
				case "band #":
					bandNumberIndex = i;
					break loop;
				case "species":
					speciesIndex = i;
					break loop;
				case "age":
					ageIndex = i;
					break loop;
				case "ha":
					howAgedIndex = i;
					break loop;
				case "ha2":
					howAged2Index = i;
					break loop;
				case "sex":
					sexIndex = i;
					break loop;
				case "hs":
					howSexedIndex = i;
					break loop;
				case "hs2":
					howSexed2Index = i;
					break loop;
				case "skull":
					skullIndex = i;
					break loop;
				case "bp":
					bpIndex = i;
					break loop;
				case "cp":
					cpIndex = i;
					break loop;
				case "fat":
					fatIndex = i;
					break loop;
				case "muscle":
					muscleIndex = i;
					break loop;
				case "b mlt":
					bMltIndex = i;
					break loop;
				case "ff mlt":
					ffMltIndex = i;
					break loop;
				case "ff wear":
					ffWearIndex = i;
					break loop;
				case "wing":
					wingIndex = i;
					break loop;
				case "tail":
					tailIndex = i;
					break loop;
				case "weight":
					weightIndex = i;
					break loop;
				case "status":
					statusIndex = i;
					break loop;
				case "date":
					dateIndex = i;
					break loop;
				case "yyyy":
					yearIndex = i;
					break loop;
				case "mm":
					monthIndex = i;
					break loop;
				case "dd":
					dayIndex = i;
					break loop;
				case "julian":
					julianIndex = i;
					break loop;
				case "cap time":
					capTimeIndex = i;
					break loop;
				case "site":
					siteIndex = i;
					break loop;
				case "net #":
					netNumberIndex = i;
					break loop;
				case "parasites?":
					parasitesIndex = i;
					break loop;
				case "disposition":
					dispIndex = i;
					break loop;
				case "notes":
					notesIndex = i;
					break loop;
				case "proofing notes":
					proofingNotesIndex = i;
					break loop;
				}
			}
		}
	}

	/**
	 * Proofs all bird variables in current data line
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAll(DataLine line) {
		proofMuscle(line);
		proofSpecies(line);
		proofAge(line);
		proofAgeFFMolt(line);
		proofHowAgedFFMolt(line);
		proofHowAgedFFWear(line);
		proofAgeFFWear(line);
		proofFFWear(line);
		proofWing(line);
		proofTail(line);
		proofWeight(line);
		proofBP(line);
		proofCP(line);
		proofFat(line);
		proofBMlt(line);
		proofSex(line);
		proofSkullAge(line);
		proofHowSexed(line);
		proofAgeBPCP(line);
		proofFFMolt(line);
		proofBandSize(line);
		proofDisposition(line);
		proofStatus(line);
		proofDisp(line);
		proofYear(line);
		proofMonth(line);
		proofDay(line);
		proofCapTime(line);
		proofNet(line);
		proofNotes(line);
		proofAgeHAHS(line);
		proofHowAged(line);
		proofParasites(line);
		proofhaha2(line);
		proofhshs2(line);
	}

	/**
	 * Checks if species is valid, refer to master species list to update
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofSpecies(DataLine line) {
		final String[] validSpecies = { "amgo", "amke", "amre", "amro", "auwa", "bade", "balo", "bcch", "bewr", "bggn",
				"bhco", "bhgr", "brcr", "brsp", "btyw", "buor", "bush", "cafi", "cahu", "canw", "caqu", "cavi", "cbch",
				"cedw", "chsp", "coha", "coni", "copo", "deju", "dowo", "dufl", "evgr", "flow", "fosp", "gcki", "gcsp",
				"grca", "grfl", "gtto", "gwcs", "hafl", "hawo", "heth", "hewa", "hofi", "howr", "lazb", "lefl", "lego",
				"lisp", "mgwa", "moch", "mwcs", "mywa", "nawa", "nofl", "nopo", "ocwa", "orju", "osfl", "pawr", "pisi",
				"rbnu", "rcki", "recr", "rnsa", "rowr", "rsfl", "sath", "savs", "sosp", "spto", "ssha", "stja", "swth",
				"tewa", "toso", "towa", "udej", "uyrw", "vath", "vesp", "wavi", "wbnu", "wcsp", "webl", "wefl", "weta",
				"wewp", "wifl", "wiwa", "ybch", "yewa", "yrwa" };
		if (!binarySearch(validSpecies, line.getSpecies().toLowerCase(), 0, validSpecies.length - 1)) {
			line.setCustomNotes(line.getProofingNotes() + "***" + line.getSpecies() + " is rare or does not exist.");
		}
	}

	/**
	 * Binary search for species
	 * 
	 * @param data
	 *            list of valid species
	 * @param key
	 *            current line's species
	 * @param lo
	 *            current left bound
	 * @param hi
	 *            current right bound
	 * @return whether species was found or not
	 */
	private boolean binarySearch(String[] data, String key, int lo, int hi) {
		int mid = (hi + lo) / 2;
		boolean found = false;
		if (data[mid].equals(key)) {
			found = true;
		} else if (lo == hi) {
			return found;
		} else if (key.compareTo(data[mid]) < 0) {
			return binarySearch(data, key, lo, mid);
		} else if (key.compareTo(data[mid]) > 0) {
			return binarySearch(data, key, mid + 1, hi);
		}
		return found;
	}

	/**
	 * View values for age. Acceptable ages are: 0,1,2,4,5,6--flag any records with
	 * blank age
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAge(DataLine line) {
		int age = line.getAge();
		if (!(age == 0 || age == 1 || age == 2 || age == 4 || age == 5 || age == 6)) {
			line.setCustomNotes(line.getProofingNotes() + "***Invalid age. Age must be 0 1 2 4 5 or 6");
		}
	}

	/**
	 * View values for sex. Acceptable values= M F U--flag all the blanks
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofSex(DataLine line) {
		String sex = line.getSex();
		if (!(sex.equals("M") || sex.equals("F") || sex.equals("U"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Invalid sex. Sex must be M F or U");
		}
	}

	/**
	 * Check that age and BP/CP match: Age 2, 4, and 0 should always have 0 for both
	 * BP and CP
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAgeBPCP(DataLine line) {
		int age = line.getAge();
		int bp = line.getBP();
		int cp = line.getCP();
		if (age == 0 || age == 2 || age == 4) {
			if (bp > 0 || cp > 0) {
				line.setCustomNotes(line.getProofingNotes() + "***BP and CP must both be 0 if age = 0 2 or 4");
			}
		}
	}

	/**
	 * View values for how aged. allowable values include: PL,
	 * EY,FF,MB,PC,LP,NL,MR,SK,TS, (blank only in second field, or for age 0)
	 * 
	 * F: PL,BP,WL--first HS field can NOT be blank, if sexed by BP, BP value cannot
	 * be blank or 0
	 * 
	 * M: PL,CL,WL--first HS field can NOT be blank, if sexed by CL, CP value cannot
	 * be blank, 0, or 1 (i.e. CP must = 2 or 3)
	 * 
	 * U: always blank, or IC, If not blank, check hard copy for errors or
	 * white-out. If sex is whited out, leave as U. Check fields above and below to
	 * make sure there's not a data entry error
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofHowSexed(DataLine line) {
		String hs = line.getHowSexed();
		String sex = line.getSex();
		int bp = line.getBP();
		int cp = line.getCP();
		if (sex.equals("F")) {
			if (!(hs.equals("PL") || hs.equals("BP") || hs.equals("WL"))) {
				line.setCustomNotes(line.getProofingNotes()
						+ "***Invalid how sexed value. Acceptable hs values for females are PL BP and WL");
			} else if (hs.equals("BP") && (bp == 0 || bp == -1)) {
				line.setCustomNotes(line.getProofingNotes() + "***If sexed by BP BP value cannot be blank or 0");
			}
		} else if (sex.equals("M")) {
			// ask about CP
			if (!(hs.equals("PL") || hs.equals("CL") || hs.equals("WL"))) {
				line.setCustomNotes(line.getProofingNotes()
						+ "***Invalid how sexed value. Acceptable hs values for males are PL CL and WL");
			} else if (hs.equals("CL")) {
				if (!(cp == 2 || cp == 3)) {
					line.setCustomNotes(line.getProofingNotes() + "***If sexed by CL CP value must be 2 or 3");
				}
			}
		} else if (sex.equals("U")) {
			if (!(hs.equals("") || hs.equals("IC"))) {
				line.setCustomNotes(line.getProofingNotes()
						+ "***Invalid how sexed value. Acceptable hs values for sex = U are blank and IC");
			}
		}

	}

	/**
	 * Check FF molt. Allowable values: N, S, J,A, blank
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofFFMolt(DataLine line) {
		String ffMlt = line.getFFMlt();
		if (!(ffMlt.equals("N") || ffMlt.equals("S") || ffMlt.equals("J") || ffMlt.equals("A") || ffMlt.equals(""))) {
			line.setCustomNotes(
					line.getProofingNotes() + "***Invalid FF molt value. Acceptable values are N S J A and blank");
		}
	}

	/**
	 * Check BP values (0-5, blank okay)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofBP(DataLine line) {
		int bp = line.getBP();
		if (bp > 5) {
			line.setCustomNotes(line.getProofingNotes() + "***BP cannot exceed 5");
		}
	}

	/**
	 * Check CP values (0-3 allowed, blank okay)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofCP(DataLine line) {
		int cp = line.getCP();
		if (cp > 3) {
			line.setCustomNotes(line.getProofingNotes() + "***CP cannot exceed 3");
		}
	}

	/**
	 * Check values for fat. 0-5, blank are allowed. 6 fat is okay but only if
	 * there's a note
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofFat(DataLine line) {
		int fat = line.getFat();
		if (fat > 5) {
			line.setCustomNotes(line.getProofingNotes() + "***Fat cannot exceed 5");
		}
	}

	/**
	 * Check values for body molt. Allowable values: 0-4, blank
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofBMlt(DataLine line) {
		int bMlt = line.getBMlt();
		if (bMlt > 4) {
			line.setCustomNotes(line.getProofingNotes() + "***Body molt cannot exceed 4");
		}
	}

	/**
	 * Check values for FF wear. Allowable values: 0-5, blank
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofFFWear(DataLine line) {
		int ffWear = line.getFFWear();
		if (ffWear > 5) {
			line.setCustomNotes(line.getProofingNotes() + "***FF wear cannot exceed 5");
		}
	}

	/**
	 * Check values for muscle. 2.5,3,4,5, blank allowed. 1 or 2 are allowed but
	 * MUST have a note, otherwise it's likely a type-o (check hard copy)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofMuscle(DataLine line) {
		double muscle = line.getMuscle();
		if (!(muscle == -1 || muscle == 2.5 || muscle == 3 || muscle == 4 || muscle == 5)) {
			if (muscle == 1 || muscle == 2) {
				if (line.getProofingNotes().equals("")) {
					line.setCustomNotes("***Muscle cannot be 1 or 2 without a note");
				}
			} else {
				line.setCustomNotes(line.getProofingNotes()
						+ "***Muscle value invalid. Value must be 2.5 3 4 or 5. 1 and 2 with notes is acceptable");
			}
		}
	}

	/**
	 * Check that skull matches with age: allowable values for skull 0-6, 8,9,
	 * blank. Flag all values in the skull column that don't match these
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofSkullAge(DataLine line) {
		int skull = line.getSkull();
		int age = line.getAge();
		String species = line.getSpecies().toLowerCase();
		String howAged = line.getHowAged().toLowerCase();
		final String[] excSpecies = { "heth", "swth", "deju", "orju", "udej", "hafl", "dufl" };
		if (skull == 0 && !(age == 2 || age == 4)) {
			line.setCustomNotes(
					line.getProofingNotes() + "***Age cannot be " + age + " when skull is 0. Age must be 2 or 4");
		} else if (skull == 1 && !(age == 2 || age == 4)) {
			line.setCustomNotes(
					line.getProofingNotes() + "***Age cannot be " + age + " when skull is 1. Age must be 2 or 4");
		} else if (age != 2 && (skull == 3 || skull == 4)) {
			line.setCustomNotes(
					line.getProofingNotes() + "***Age cannot be " + age + " when skull is 3 or 4. Age must be 2");
		} else if (skull == 5 && age != 2) {
			if (age == 1 || age == 5 || age == 6) {
				// is exception species?
				boolean contains = false;
				for (String sp : excSpecies) {
					if (species.equals(sp)) {
						contains = true;
					}
				}
				if (!contains) {
					line.setCustomNotes(line.getProofingNotes() + "***Age cannot be " + age
							+ " when skull is 5 and species is not an exception. Age must be 2");
				}
			} else {
				line.setCustomNotes(
						line.getProofingNotes() + "***Age cannot be " + age + " when skull is 5. Age must be 2");
			}
		} else if (skull == 6 && !(age == 1 || age == 5 || age == 6)) {
			if (!((age == 2 || age == 0) && species.equals("rcki"))) {
				line.setCustomNotes(line.getProofingNotes() + "***Age cannot be " + age
						+ " when skull is 6. Age must be 1 5 6 or 2 with rcki species");
			}
		} else if (howAged.equals("sk") && (skull == 7 || skull == 8)) {
			line.setCustomNotes(line.getProofingNotes() + "***How Aged cannot be skull when skull = 7 8");
		}
	}

	/**
	 * View values for Location. Make sure there are no blanks
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofLocation(DataLine line) {
		String location = line.getLocation();
		if (location.equals("")) {
			line.setCustomNotes(line.getProofingNotes() + "***Location cannot be empty");
		}
	}

	/**
	 * View values for band size. Make sure there are no blanks. Make sure the only
	 * values used are 0A, 0, 1, 1B, 1A, 1C, 2, 3, 3A, 3B
	 * 
	 * @param line
	 */
	public void proofBandSize(DataLine line) {
		String bandSize = line.getBandSize();
		String disposition = line.getDisposition();
		if (!(bandSize.equals("") || bandSize.equals("0A") || bandSize.equals("0") || bandSize.equals("1")
				|| bandSize.equals("1B") || bandSize.equals("1A") || bandSize.equals("1C") || bandSize.equals("2")
				|| bandSize.equals("3") || bandSize.equals("3A") || bandSize.equals("3B"))) {
			if (bandSize.equals("R")) {
				if (!disposition.equals("R")) {
					line.setCustomNotes(line.getProofingNotes() + "***Band size cannot be R when band code is not R");
				}
			} else if (bandSize.equals("U")) {
				if (!disposition.equals("U")) {
					line.setCustomNotes(line.getProofingNotes() + "***Band size cannot be U when band code is not U");
				}
			} else {
				line.setCustomNotes(
						line.getProofingNotes() + "***Band size invalid. Acceptable values: 0A 0 1 1B 1A 2 3 3A 3B");
			}
		}
	}

	/**
	 * Check that FF molt matches with age. blanks are okay, and can match with any
	 * age. Refer to table on rules page
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAgeFFMolt(DataLine line) {
		int age = line.getAge();
		String ffMlt = line.getFFMlt();
		String species = line.getSpecies().toLowerCase();
		if ((age == 1 || age == 5 || age == 6)
				&& !(ffMlt.equals("N") || ffMlt.equals("S") || ffMlt.equals("A") || ffMlt.equals(""))) {
			line.setCustomNotes(line.getProofingNotes() + "***Valid FFMlt values for ages 156 are NSA");
		} else if (age == 2 && !(ffMlt.equals("N") || ffMlt.equals("A") || ffMlt.equals("J") || ffMlt.equals(""))) {
			if (!(species.equals("ybch") || species.equals("spto") || species.equals("sosp") || species.equals("hofi")
					|| species.equals("nofl") || species.equals("rsfl") || species.equals("hawo"))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***Valid FFMlt values for age 2 are NA if not an exception species");
			}
		} else if (age == 4 && !(ffMlt.equals("J") || ffMlt.equals(""))) {
			line.setCustomNotes(line.getProofingNotes() + "***If age = 4 then FFMlt must be J");
		}
	}

	/**
	 * if "how aged" says MR, FF molt must be S or J (can't be blank, N, or A)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofHowAgedFFMolt(DataLine line) {
		String ha = line.getHowAged().toLowerCase();
		String ffMlt = line.getFFMlt();
		if (ha.equals("mr") && !(ffMlt.equals("S") || ffMlt.equals("J"))) {
			line.setCustomNotes(line.getProofingNotes() + "***If How Aged is MR then FFMolt cannot be blank");
		}
	}

	/**
	 * If "how aged" says FF then FF Wear cannot be blank
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofHowAgedFFWear(DataLine line) {
		String ha = line.getHowAged().toLowerCase();
		int ffWear = line.getFFWear();
		if (ha.equals("ff") && ffWear < 0) {
			line.setCustomNotes(line.getProofingNotes() + "***If How Aged is FF then FFWear cannot be blank");
		}
	}

	/**
	 * 0 or 1 FF wear is highly suspicious for age 5 and 6. Flag all these records
	 * (Sometimes 0 FF wear is normal if paired with S FF molt, but then
	 * micro-ageing is suspect, so we should flag the record either way...maybe with
	 * a message "FF wear and age combination unlikely. Check this record"
	 * 
	 * 2+ FF wear is suspicious for age 4--add message "age and FF wear combination
	 * unlikely"
	 * 
	 * 4+ is suspicious for age 2--add "unlikely" message
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAgeFFWear(DataLine line) {
		int age = line.getAge();
		int ffWear = line.getFFWear();
		String species = line.getSpecies();
		if ((age == 5 || age == 6) && (ffWear == 0 || ffWear == 1)) {
			line.setCustomNotes(
					line.getProofingNotes() + "***FF wear of 0 or 1 is suspicious for a bird of age 5 or 6.");
		} else if (age == 4 && ffWear >= 2) {
			line.setCustomNotes(line.getProofingNotes() + "***FF wear >= 2 is suspicious for a bird of age 4");
		} else if (age == 2 && ffWear >= 4) {
			line.setCustomNotes(line.getProofingNotes() + "***FF wear >= 4 is suspicious for a bird of age 2");
		}
	}

	/**
	 * Check if wing is below 30 or above 200
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofWing(DataLine line) {
		int wing = line.getWing();
		if (wing > -1) {
			if (wing < 30 || wing > 200) {
				line.setCustomNotes(line.getProofingNotes() + "***Wing below 30 or above 200 is suspicious");
			}
		}
	}

	/**
	 * Check if tail is below 30 or above 200
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofTail(DataLine line) {
		int tail = line.getTail();
		if (tail > -1) {
			if (tail < 30 || tail > 200) {
				line.setCustomNotes(line.getProofingNotes() + "***Tail below 30 or above 200 is suspicious");
			}
		}
	}

	/**
	 * anything under 5 (but GCKI or BCHU RUHU CAHU okay) or over 200 (raptors would
	 * be a rare exception)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofWeight(DataLine line) {
		String species = line.getSpecies().toLowerCase();
		double weight = line.getWeight();
		if (weight > -1) {
			if (weight < 5) {
				if (!(species.equals("rcki") || species.equals("bchu") || species.equals("ruhu")
						|| species.equals("cahu"))) {
					line.setCustomNotes(line.getProofingNotes() + "***" + weight
							+ " weight is suspicious for a bird that is not rcki bchu ruhu or cahu");
				}
			} else if (weight > 200) {
				if (!species.equals("coha")) {
					line.setCustomNotes(line.getProofingNotes() + "***" + weight
							+ " weight is suspicious for a bird that isn't a raptor");
				}
			}
		}
	}

	/**
	 * View values for band code (also called DISPOSITION). Make sure there are no
	 * blanks. Make sure the only values used are 1,R,4,5,8,R,U. Make sure 4 and 8
	 * are only used for species codes BADE and BALO
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofDisposition(DataLine line) {
		String disp = line.getDisposition();
		String species = line.getSpecies().toLowerCase();
		if (disp.equals("")) {
			line.setCustomNotes(line.getProofingNotes() + "***Band code cannot be empty");
		} else if (disp.equals("8") || disp.equals("4")) {
			if (!(species.equals("bade") || species.equals("balo"))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***Band codes 4 and 8 are only valid if species is BADE or BALO");
			}
		} else if (!(disp.equals("1") || disp.equals("5") || disp.equals("R") || disp.equals("U"))) {
			line.setCustomNotes(line.getProofingNotes()
					+ "***Invalid band code. Acceptable values: 1 5 R U or 4 and 8 if species = BADE BALO");
		} else if (species.equals("bade") || species.equals("balo")) {
			if (!(disp.equals("8") || disp.equals("4"))) {
				line.setCustomNotes(line.getProofingNotes() + "***If band was destroyed or lost, disp must be 4 or 8");
			}

		}
	}

	/**
	 * Check status. Allowable values for new bands: 300, 500. Blank is NOT valid
	 * ALL status 500's MUST have text in the note column and a letter in the disp
	 * column (i.e. Note and Disp columns cannot be blank)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofStatus(DataLine line) {
		int status = line.getStatus();
		String notes = line.getNotes();
		String disp = line.getDisp();
		if (!(status == 300 || status == 500 || status == 0)) {
			if (status == -1) {
				line.setCustomNotes(line.getProofingNotes() + "***Status field cannot be blank");
			} else {
				line.setCustomNotes(
						line.getProofingNotes() + "***" + status + " is an invalid status must be 300 or 500");
			}
		} else if (status == 500 && notes.equals("") && disp.equals("")) {
			line.setCustomNotes(line.getProofingNotes() + "***Notes and disp cannot be blank when status is 500");
		}
	}

	/**
	 * Check DISP. Allowable values include: M,O,I,S,E,D,T,W,B,L,P, blank Any bird
	 * with a letter in disp should have a note explaining why and the status should
	 * say 500
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofDisp(DataLine line) {
		String disp = line.getDisp();
		int status = line.getStatus();
		String notes = line.getNotes();
		if (disp.equals("M") || disp.equals("O") || disp.equals("I") || disp.equals("S") || disp.equals("E")
				|| disp.equals("D") || disp.equals("T") || disp.equals("W") || disp.equals("B") || disp.equals("L")
				|| disp.equals("P") || disp.equals("")) {
			if (!disp.equals("")) {
				if (status != 500 && !disp.equals("D")) {
					line.setCustomNotes(line.getProofingNotes() + "***If disp field is not empty status must be 500");
				}
				if (notes.equals("")) {
					line.setCustomNotes(line.getProofingNotes() + "***If disp field is not empty notes must be filled");
				}
			}
		} else {
			line.setCustomNotes(line.getProofingNotes() + "***" + disp + " is an invalid disp value");
		}
	}

	// proof date later. use date class?
	/**
	 * Check values for YYYY. No blanks. Allowable values are any valid year between
	 * 1997 and current year (except BADE BALO)
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofYear(DataLine line) {
		int year = line.getYear();
		String species = line.getSpecies().toLowerCase();
		if (year == -1 && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Year cannot be blank if band wasn't lost or destroyed");
		} else if (year < 1997 && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Year cannot be before 1997");
		}
	}

	/**
	 * Check values for MM. valid: 2-11. no blanks except for BADE BALO
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofMonth(DataLine line) {
		int month = line.getMonth();
		String species = line.getSpecies().toLowerCase();
		if (month == -1 && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Month cannot be blank if band wasn't lost or destroyed");
		} else if ((month < 2 || month > 11) && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Month cannot be December or January");
		}
	}

	/**
	 * Check values for DD. Valid: 1-31. no blanks except for BADE/BALO
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofDay(DataLine line) {
		int day = line.getDay();
		String species = line.getSpecies().toLowerCase();
		if (day == -1 && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Day cannot be blank if band wasn't lost or destroyed");
		} else if ((day < 1 || day > 31) && !(species.equals("bade") || species.equals("balo"))) {
			line.setCustomNotes(line.getProofingNotes() + "***Day cannot be < 1 or > 31");
		}
	}

	/**
	 * check values for cap time. Allowed values include: 650 to 1300. Flag all
	 * other values (Other values may happen only if there is a note (sometimes
	 * songbirds are caught during owls, hawk trapping, etc). All values should end
	 * in 0's
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofCapTime(DataLine line) {
		int capTime = line.getCapTime();
		if (capTime < 650 || capTime > 1300) {
			line.setCustomNotes(line.getProofingNotes() + "***Cap time must be between 650 and 1300");
		} else if (capTime % 10 != 0) {
			line.setCustomNotes(line.getProofingNotes() + "***Cap time must end in 0");
		}
	}

	/**
	 * Check values for net. Allowable values: 1-10, blank. Some exceptions allowed
	 * with a note (e.g. owl nets) but we should flag those exceptions anyway to
	 * make sure someone checks them
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofNet(DataLine line) {
		int net = line.getNetNum();
		if (net != -1) {
			if (net < 1 || net > 10) {
				line.setCustomNotes(line.getProofingNotes() + "***Net # must be between 1 and 10");
			}
		}
	}

	/**
	 * Check that notes that mention either "FF" "flat flies" or "mites" "lice"
	 * "louse" "mite" "fly" have a Y for parasite column
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofNotes(DataLine line) {
		String notes = line.getNotes().toLowerCase();
		if (notes.contains(" ff ") || notes.contains(" flat flies ") || notes.contains(" mites ")
				|| notes.contains(" lice ") || notes.contains(" louse ") || notes.contains(" mite ")
				|| notes.contains(" fly ")) {
			if (!line.hasParasites()) {
				line.setCustomNotes(line.getProofingNotes() + "***Read notes and change parasites to Y if warranted");
			}
		}
	}

	/**
	 * EY in the How Aged columns should only be used for species codes SPTO, DOWO,
	 * NOFL, RSFL, HAWO, DEJU, ORJU, SCJU, UDEJ --flag any other species that use
	 * this with note "Check in Pyle to confirm that this species can be aged by eye
	 * color"
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofHowAged(DataLine line) {
		String species = line.getSpecies().toLowerCase();
		String ha = line.getHowAged().toLowerCase();
		String ha2 = line.getHowAged2().toLowerCase();
		if ((ha.equals("ey") || ha2.equals("ey")) && !(species.equals("spto") || species.equals("dowo")
				|| species.equals("nofl") || species.equals("rsfl") || species.equals("hawo") || species.equals("deju")
				|| species.equals("orju") || species.equals("scju") || species.equals("udej"))) {
			line.setCustomNotes(
					line.getProofingNotes() + "***Check in Pyle to confirm that this species can be aged by eye color");
		}
	}

	/**
	 * Check that age, how aged, and how sexed match
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofAgeHAHS(DataLine line) {
		int age = line.getAge();
		String ha = line.getHowAged();
		String ha2 = line.getHowAged2();
		String hs = line.getHowSexed();
		String species = line.getSpecies().toLowerCase();
		if (age == 0) {
			if (!(ha.equals("") || ha.equals("IC"))) {
				line.setCustomNotes(line.getProofingNotes() + "***invalid how aged for age 0");

			}
			if (!(hs.equals("") || hs.equals("IC"))) {
				if (!(species.equals("rcki") || species.equals("gcki"))) {
					line.setCustomNotes(line.getProofingNotes()
							+ "***known sex for a bird of unknown age is unlikely. exceptions = rcki gcki");
				}
			}
		} else if (age == 1 || age == 6) {
			// "NL" should always be in first box for age 6.
			// Other options for the HA column for age 6 and 1: PL NL EY FF MB PC MR
			// SK TS blank in second field only
			if (!ha.equals("NL") && age == 6) {
				line.setCustomNotes(line.getProofingNotes() + "***how aged must be NL if age = 6");
			} else if (age == 1 && !(ha.equals("PL") || ha.equals("NL") || ha.equals("EY") || ha.equals("FF")
					|| ha.equals("MB") || ha.equals("PC") || ha.equals("MR") || ha.equals("FB") || ha.equals("SK")
					|| ha.equals("TS") || ha.equals("BP"))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how aged must be PL NL EY FF MB PC MR SK or TS if age = 1");
			}
			// PLBPCLWL blank
			if (!(hs.equals("PL") || hs.equals("BP") || hs.equals("CL") || hs.equals("WL") || hs.equals(""))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how sexed must be PL BP CL WL or blank if age = 1 or 6");
			}
		} else if (age == 5) {
			// "LP" MUST be in the first "how aged" field for all age 5's.
			// 2nd "how aged" field may include: PL EY FF MB PC MR SK TS or blank
			if (!ha.equals("LP")) {
				line.setCustomNotes(line.getProofingNotes() + "***how aged must be LP in the first column if age = 5");
			}
			if (!(ha2.equals("PL") || ha2.equals("EY") || ha2.equals("FF") || ha2.equals("MB") || ha2.equals("PC")
					|| ha2.equals("MR") || ha2.equals("SK") || ha2.equals("BP") || ha2.equals("TS")
					|| ha2.equals(""))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how aged must be PL EY FF MB PC MR SK TS or blank if age = 5");
			}
			// PLBPCLWL blank
			if (!(hs.equals("PL") || hs.equals("BP") || hs.equals("CL") || hs.equals("WL") || hs.equals(""))) {
				line.setCustomNotes(line.getProofingNotes() + "***how sexed must be PL BP CL WL or blank if age = 5");
			}
		} else if (age == 2) {
			// PLEYFFMBPCLPMRSKTS (blank allowed in second field only)
			if (!(ha.equals("PL") || ha.equals("EY") || ha.equals("FF") || ha.equals("MB") || ha.equals("PC")
					|| ha.equals("LP") || ha.equals("MR") || ha.equals("SK") || ha.equals("TS"))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how aged must be PL EY FF MB PC LP MR SK or TS if age = 2");
			}
			// if sex = U hs can be blank
			if (!(hs.equals("PL") || hs.equals("WL") || hs.equals(""))) {
				line.setCustomNotes(line.getProofingNotes() + "***how sexed must be PL or WL if age = 2");
			}
		} else if (age == 4) {
			// The first "how aged" field MUST be "MR" and FF molt MUST be "J". 2nd "how
			// aged" field may be: PLEYFFMBPCLPSKTS blank
			if (!(ha.equals("MR") || ha.equals("NF") && line.getFFMlt().equals("J"))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how aged must be MR and ff molt must be J if age = 4");
			}
			if (!(ha2.equals("PL") || ha2.equals("EY") || ha2.equals("FF") || ha2.equals("MB") || ha2.equals("PC")
					|| ha2.equals("LP") || ha2.equals("SK") || ha2.equals("PC") || ha2.equals("TS")
					|| ha2.equals(""))) {
				line.setCustomNotes(
						line.getProofingNotes() + "***how aged 2 must be PL EY FF MB PC LP SK TS or blank if age = 2");
			}
		}

	}

	/**
	 * If there is a Y in the parasites column there needs to be a note
	 * 
	 * @param line
	 *            current line to be proofed
	 */
	public void proofParasites(DataLine line) {
		boolean parasites = line.hasParasites();
		String notes = line.getNotes();
		if (parasites && notes.equals("")) {
			line.setCustomNotes(
					line.getProofingNotes() + "If there is a Y in the parasites column there needs to be a note");
		}
	}

	public void proofhaha2(DataLine line) {
		String ha = line.getHowAged();
		String ha2 = line.getHowAged2();
		if (!(ha.equals("") || ha2.equals(""))) {
			if (ha.equals(ha2)) {
				line.setCustomNotes(
						line.getProofingNotes() + "***if both how aged columns are filled both cannot be the same");
			}
		}
	}

	public void proofhshs2(DataLine line) {
		String hs = line.getHowSexed();
		String hs2 = line.getHowSexed2();
		if (!(hs.equals("") || hs2.equals(""))) {
			if (hs.equals(hs2)) {
				line.setCustomNotes(
						line.getProofingNotes() + "***if both how sexed columns are filled both cannot be the same");
			}
		}
	}
}