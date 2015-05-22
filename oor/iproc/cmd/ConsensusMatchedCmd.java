/**
 * 
 */
package oor.iproc.cmd;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import mpicbg.models.AbstractAffineModel2D;
import mpicbg.models.AffineModel2D;
import mpicbg.models.PointMatch;
import mpicbg.models.RigidModel2D;
import mpicbg.models.SimilarityModel2D;
import mpicbg.models.TranslationModel2D;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class ConsensusMatchedCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public boolean execute(Context ctxt) throws Exception {
		AbstractAffineModel2D<?> model = null;
		int modelType = getIntValue(ctxt, CM_MODEL);
		
		switch (modelType) {
		case 1:
			model = new AffineModel2D();
			break;
		case 2:
			model = new RigidModel2D();
			break;
		case 3:
			model = new SimilarityModel2D();
			break;
		case 4:
			model = new TranslationModel2D();
			break;
		}
		
		List<PointMatch> candidates = new ArrayList<PointMatch>((Collection<PointMatch>)ctxt.get(CM_CANDIDATES));
		Collection<PointMatch> inliers = new ArrayList<PointMatch>();
		int iterations = 1000;
		float maxEpsilon = getFloatValue(ctxt, CM_MAXEPS);
		float minInlierRatio = getFloatValue(ctxt, CM_MINIR);
		if (model != null)
			model.filterRansac(candidates, inliers, iterations, maxEpsilon, minInlierRatio);
		
		ctxt.put(CM_INLIERS, inliers);
		return false;
	}

}
