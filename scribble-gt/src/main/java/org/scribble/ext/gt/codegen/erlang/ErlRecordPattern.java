package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class ErlRecordPattern implements ErlTerm {
    private final String recordName;
    private final Map<String, ErlTerm> fields;

    public ErlRecordPattern(String recordName, Map<String, ErlTerm> fields) {
        this.recordName = recordName;
        // preserve insertion order.
        this.fields = new LinkedHashMap<>(fields);
    }

    @Override
    public void write(FileWriter w) throws IOException {
        w.write("#" + recordName + "{");
        Iterator<Entry<String, ErlTerm>> iter = fields.entrySet().iterator();
        while (iter.hasNext()) {
            Entry<String, ErlTerm> entry = iter.next();
            w.write(entry.getKey() + " = ");
            entry.getValue().write(w);
            if (iter.hasNext()) {
                w.write(", ");
            }
        }
        w.write("}");
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("#").append(recordName).append("{");
        Iterator<Entry<String, ErlTerm>> iter = fields.entrySet().iterator();
        while (iter.hasNext()) {
            Entry<String, ErlTerm> entry = iter.next();
            sb.append(entry.getKey()).append(" = ").append(entry.getValue().toString());
            if (iter.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
}
