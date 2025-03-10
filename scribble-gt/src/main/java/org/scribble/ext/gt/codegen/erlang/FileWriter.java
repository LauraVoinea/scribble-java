package org.scribble.ext.gt.codegen.erlang;
//
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.nio.file.StandardOpenOption;
//
///**
// * Utility class for writing generated Erlang code to files.
// */
//public final class FileWriter {
//
//    private static final String OUTPUT_DIR = "./test";
//    private static final String ERL_EXTENSION = ".erl";
//    private static final String HEADER_EXTENSION = ".hrl";
//    private static final String DOT_EXTENSION = ".dot";
//
//    // Private constructor to prevent instantiation.
//    private FileWriter() {}
//
//    /**
//     * Writes the generated Erlang module content to a file.
//     *
//     * @param protocolName the protocol name used as a subdirectory.
//     * @param moduleName   the name of the Erlang module (without extension).
//     * @param content      the generated Erlang code.
//     * @throws IOException if an I/O error occurs.
//     */
//    public static void writeErlFile(String protocolName, String moduleName, String content) throws IOException {
//        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName);
//        Files.createDirectories(outputDirectory);
//
//        Path filePath = outputDirectory.resolve(moduleName + ERL_EXTENSION);
//        Files.writeString(filePath, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
//    }
//
//    /**
//     * Writes the generated Erlang header file content to a file.
//     *
//     * @param protocolName the protocol name used as a subdirectory.
//     * @param moduleName   the name of the header file (without extension).
//     * @param content      the generated Erlang header code.
//     * @throws IOException if an I/O error occurs.
//     */
//    public static void writeHeaderFile(String protocolName, String moduleName, String content) throws IOException {
//        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName);
//        Files.createDirectories(outputDirectory);
//
//        Path filePath = outputDirectory.resolve(moduleName + HEADER_EXTENSION);
//        Files.writeString(filePath, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
//    }
//
//    /**
//     * Writes the given digraph content to a DOT file.
//     *
//     * @param protocolName the protocol name used as a subdirectory.
//     * @param moduleName   the base name of the dot file (without extension).
//     * @param content      the digraph content in DOT format.
//     * @throws IOException if an I/O error occurs.
//     */
//    public static void writeDotFile(String protocolName, String moduleName, String content) throws IOException {
//        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName);
//        Files.createDirectories(outputDirectory);
//
//        Path filePath = outputDirectory.resolve(moduleName + DOT_EXTENSION);
//        Files.writeString(filePath, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
//    }
//}

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class FileWriter {
    private final BufferedWriter writer;
    private final StringBuilder buffer;
    private int indentLevel;
    private boolean atLineStart;
    private static final String INDENT_UNIT = "    ";  // 4 spaces for each indent level

    public FileWriter(Path filePath) throws IOException {
        // Open the file for writing (UTF-8 encoding)
        this.writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(String.valueOf(filePath)), StandardCharsets.UTF_8));
        this.buffer = new StringBuilder();
        this.indentLevel = 0;
        this.atLineStart = true;
    }

    /** Increase indent level for subsequent lines */
    public void indent() {
        indentLevel++;
    }

    /** Decrease indent level (not going below 0) */
    public void dedent() {
        if (indentLevel > 0) {
            indentLevel--;
        }
    }

    /** Write text without adding a newline (indentation applied at line start). */
    public void write(String text) throws IOException {
        if (text == null || text.isEmpty()) {
            return;  // nothing to write
        }
        for (int i = 0; i < text.length(); i++) {
            char ch = text.charAt(i);
            if (atLineStart) {
                // Write indentation for new line
                for (int j = 0; j < indentLevel; j++) {
                    writer.write(INDENT_UNIT);
                    buffer.append(INDENT_UNIT);
                }
                atLineStart = false;
            }
            writer.write(ch);
            buffer.append(ch);
            if (ch == '\n') {
                // If a newline character is encountered, mark next char as line start
                atLineStart = true;
            }
        }
    }

    /** Write text followed by a newline (indentation applied, then newline). */
    public void writeLine(String text) throws IOException {
        if (text != null) {
            write(text);
        }
        // Always terminate the line with a newline (if not already ended with one)
        if (text == null || text.isEmpty() || text.charAt(text.length() - 1) != '\n') {
            writer.newLine();
            buffer.append(System.lineSeparator());
        }
        atLineStart = true;
    }

    /** Close the file writer, releasing resources. */
    public void close() throws IOException {
        writer.flush();
        writer.close();
    }

    /** Returns the collected text written to the file (for debugging). */
    @Override
    public String toString() {
        return buffer.toString();
    }
}
