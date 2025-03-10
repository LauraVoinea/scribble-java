package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents updating (or creating) an Erlang record (Record#RecordName{Field=Value, ...}). */
public class ErlRecordUpdate implements ErlTerm {
    private ErlTerm baseRecord; // null if creating a new record
    private String recordName;
    private LinkedHashMap<String, ErlTerm> fields = new LinkedHashMap<>();

    public ErlRecordUpdate(ErlTerm baseRecord, String recordName) {
        this.baseRecord = baseRecord;
        this.recordName = recordName;
    }
    /** Add a field assignment for the record update. */
    public void addField(String field, ErlTerm value) {
        fields.put(field, value);
    }
    @Override
    public void write(FileWriter w) throws IOException {
        if (baseRecord == null) {
            w.write("#" + recordName + "{");
        } else {
            baseRecord.write(w);
            w.write("#" + recordName + "{");
        }
        Iterator<Map.Entry<String, ErlTerm>> it = fields.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, ErlTerm> entry = it.next();
            w.write(entry.getKey() + " = ");
            entry.getValue().write(w);
            if (it.hasNext()) {
                w.write(", ");
            }
        }
        w.write("}");
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (baseRecord == null) {
            sb.append("#").append(recordName).append("{");
        } else {
            sb.append(baseRecord.toString()).append("#").append(recordName).append("{");
        }
        Iterator<Map.Entry<String, ErlTerm>> it = fields.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, ErlTerm> entry = it.next();
            sb.append(entry.getKey()).append(" = ").append(entry.getValue().toString());
            if (it.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
}
