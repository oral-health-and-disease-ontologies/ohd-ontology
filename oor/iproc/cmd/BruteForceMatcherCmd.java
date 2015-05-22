/**
 * 
 */
package oor.iproc.cmd;

import java.util.Collection;
import java.util.List;

import mpicbg.imagefeatures.Feature;
import mpicbg.imagefeatures.FloatArray2DMOPS;
import mpicbg.imagefeatures.FloatArray2DSIFT;
import mpicbg.models.PointMatch;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class BruteForceMatcherCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public boolean execute(Context ctxt) throws Exception {
		List<Feature> fs1 = (List<Feature>) ctxt.get(TGT_FEATURES);
		List<Feature> fs2 = (List<Feature>) ctxt.get(SRC_FEATURES);
		float rod = getFloatValue(ctxt, FM_ROD);
		String algorithm = getValue(ctxt, FM_ALGORITHM);
		Collection<PointMatch> matches = null;
		
		if ("MOPS".equals(algorithm))
				matches = FloatArray2DMOPS.createMatches(fs1, fs2, 1.5f, null, Float.MAX_VALUE, rod);
		else if ("SIFT".equals(algorithm))
				matches = FloatArray2DSIFT.createMatches(fs1, fs2, 1.5f, null, Float.MAX_VALUE, rod);
		
		ctxt.put(FM_BRUTEFTRS, matches);
		return false;
	}

}
