/**
 * 
 */
package oor.iproc.cmd;

import ij.process.ImageProcessor;
import mmorpho.Constants;
import mmorpho.MorphoProcessor;
import mmorpho.StructureElement;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class MorphologyCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "%Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see org.apache.commons.chain.Command#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		//parameters needed
		/*
		 * Structure element
		 * 		type : i/p from user
		 * 		shift : always 1
		 * 		radius : user i/p
		 * 		offset : always offset0
		 * MorphoProcessor
		 * 		operation : user i/p
		 */
		int type = getIntValue(ctxt, MP_TYPE);
		float radius = getFLoatValue(ctxt, MP_RADIUS);
		int command = getIntValue(ctxt, MP_CMD);
		ImageProcessor ip = getTarget(ctxt);
		StructureElement se = new StructureElement(type, 1, radius, Constants.OFFSET0);
		MorphoProcessor morpho = new MorphoProcessor(se);
		switch (command) {
		case 1:
			morpho.close(ip);
			break;
		case 2:
			morpho.dilate(ip);
			break;
		case 3:
			morpho.erode(ip);
			break;
		case 4:
			morpho.fastDilate(ip);
			break;
		case 5:
			morpho.fastErode(ip);
			break;
		case 6:
			morpho.fclose(ip);
			break;
		case 7:
			morpho.fopen(ip);
			break;
		case 8:
			morpho.open(ip);
			break;
		}
		
		setTarget(ctxt, ip);
		return false;
	}

}
