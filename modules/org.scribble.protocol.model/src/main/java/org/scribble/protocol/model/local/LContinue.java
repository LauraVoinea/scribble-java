/*
 * Copyright 2009 www.scribble.org
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.scribble.protocol.model.local;

/**
 * This class represents the Raise construct.
 * 
 */
public class LContinue extends LActivity {

    private String _label=null;

    /**
     * This is the default constructor.
     * 
     */
    public LContinue() {
    }
    
    /**
     * This is the copy constructor.
     * 
     * @param copy The recursion to copy
     */
    public LContinue(LContinue copy) {
        super(copy);
        _label = copy.getLabel();
    }
    
    /**
     * This method returns the label associated with the recursion construct.
     * 
     * @return The label
     */
    public String getLabel() {
        return (_label);
    }
    
    /**
     * This method sets the label associated with the recursion construct.
     * 
     * @param label The label
     */
    public void setLabel(String label) {
        _label = label;
    }
        
    /**
     * This method visits the model object using the supplied
     * visitor.
     * 
     * @param visitor The visitor
     */
    public void visit(LVisitor visitor) {
        visitor.accept(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        LContinue that = (LContinue) o;

        return !(_label != null
                ? !_label.equals(that._label)
                : that._label != null);
    }

    @Override
    public int hashCode() {
        return _label != null ? _label.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "continue " + _label;
    }
    
	/**
	 * {@inheritDoc}
	 */
    public void toText(StringBuffer buf, int level) {
		
    	indent(buf, level);
    	
    	buf.append("continue ");
    	
    	if (_label != null) {
    		buf.append(_label);
    	}
    	
		buf.append(";\n");
	}
}
