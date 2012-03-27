/**
 * 
 */
package oor.iproc.cmd;

import java.util.ArrayList;

import mpicbg.ij.MOPS;
import mpicbg.imagefeatures.Feature;
import mpicbg.imagefeatures.FloatArray2DMOPS;
import mpicbg.imagefeatures.FloatArray2DMOPS.Param;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class MOPSExtractFeaturesCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "%Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		Param p = new Param();
		p.fdSize = getIntValue(ctxt, MOF_FDSZ);
		p.initialSigma = getFLoatValue(ctxt, MOF_INITSIG);
		p.maxOctaveSize = getIntValue(ctxt, MOF_MAXOCT);
		p.minOctaveSize = getIntValue(ctxt, MOF_MINOCT);
		p.steps = getIntValue(ctxt, MOF_STEPS);
		MOPS mops = new MOPS(new FloatArray2DMOPS(p));
		ArrayList<Feature> features = new ArrayList<Feature>();
		mops.extractFeatures(getTarget(ctxt), features);
		setTargetFeatures(ctxt, features);
		return false;
	}

}
