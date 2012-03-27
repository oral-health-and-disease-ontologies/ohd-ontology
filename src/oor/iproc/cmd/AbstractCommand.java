/**
 * 
 */
package oor.iproc.cmd;

import java.util.ArrayList;

import ij.process.ImageProcessor;
import mpicbg.imagefeatures.Feature;
import oor.iproc.ContextConstants;

import org.apache.commons.chain.Command;
import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public abstract class AbstractCommand implements Command, ContextConstants {

	/* (non-Javadoc)
	 * @see org.apache.commons.chain.Command#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public abstract boolean execute(Context arg0) throws Exception;
	
	protected String getValue(Context ctxt, String key) {
		Object obj = ctxt.get(key);
		return (obj != null) ? obj.toString() : "";
	}
	
	protected int getIntValue(Context ctxt, String key) {
		return Integer.valueOf(getValue(ctxt, key));
	}
	
	protected float getFLoatValue(Context ctxt, String key) {
		return Float.valueOf(getValue(ctxt, key));
	}
	
	protected double getDblValue(Context ctxt, String key) {
		return Double.valueOf(getValue(ctxt, key));
	}
	
	protected long getLongValue(Context ctxt, String key) {
		return Long.valueOf(getValue(ctxt, key));
	}
	
	protected boolean getBoolValue(Context ctxt, String key) {
		return Boolean.valueOf(getValue(ctxt, key));
	}
	
	protected ImageProcessor getTarget(Context ctxt) {
		return (ImageProcessor) ctxt.get(TGT_IMG);
	}
	
	@SuppressWarnings("unchecked")
	protected void setTarget(Context ctxt, ImageProcessor iproc) {
		ctxt.put(TGT_IMG, iproc);
	}
	
	protected ImageProcessor getSource(Context ctxt) {
		return (ImageProcessor) ctxt.get(SRC_IMG);
	}
	
	@SuppressWarnings("unchecked")
	protected void setSource(Context ctxt, ImageProcessor iproc) {
		ctxt.put(SRC_IMG, iproc);
	}
	
	@SuppressWarnings("unchecked")
	protected ArrayList<Feature> getSourceFeatures(Context ctxt) {
		return (ArrayList<Feature>) ctxt.get(SRC_FEATURES);
	}
	
	@SuppressWarnings("unchecked")
	protected void setSourceFeatures(Context ctxt, ArrayList<Feature> features) {
		ctxt.put(SRC_FEATURES, features);
	}
	
	@SuppressWarnings("unchecked")
	protected ArrayList<Feature> getTargetFeatures(Context ctxt) {
		return (ArrayList<Feature>) ctxt.get(TGT_FEATURES);
	}
	
	@SuppressWarnings("unchecked")
	protected void setTargetFeatures(Context ctxt, ArrayList<Feature> features) {
		ctxt.put(TGT_FEATURES, features);
	}
}
