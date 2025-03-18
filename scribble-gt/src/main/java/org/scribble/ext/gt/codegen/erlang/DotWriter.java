package org.scribble.ext.gt.codegen.erlang;

import org.scribble.ext.gt.core.model.efsm.GTEFSM;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.IOException;

public class DotWriter {

    public void writeDotFile(GTEFSM efsm, String outputDirStr, String role) throws IOException {
        // Ensure the output directory exists.
        Path outputDir = Paths.get(outputDirStr);
        Files.createDirectories(outputDir);
        // Create the output file path.
        Path dotFile = outputDir.resolve(role + ".dot");
        // Get the DOT representation.
        String dotContent = efsm.toDot();
        // Write the content to file.
        Files.writeString(dotFile, dotContent);
    }
}
