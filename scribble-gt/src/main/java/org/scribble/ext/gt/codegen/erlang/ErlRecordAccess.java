package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Represents access to a field of an Erlang record (Record#RecordName.Field). */
public class ErlRecordAccess implements ErlTerm {
    private ErlTerm recordTerm;
    private String recordName;
    private String fieldName;
    public ErlRecordAccess(ErlTerm recordTerm, String recordName, String fieldName) {
        this.recordTerm = recordTerm;
        this.recordName = recordName;
        this.fieldName = fieldName;
    }
    @Override
    public void write(FileWriter w) throws IOException {
        recordTerm.write(w);
        w.write("#" + recordName + "." + fieldName);
    }
    @Override
    public String toString() {
        return recordTerm.toString() + "#" + recordName + "." + fieldName;
    }
}
