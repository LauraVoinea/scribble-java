package org.scribble.gt.codegen;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTLConfig;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.gt.codegen.StateM;
import org.scribble.gt.codegen.GenericBehaviour;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.scribble.gt.codegen.CallbackModule.genCallbackModule;
import static org.scribble.gt.codegen.ErlangCodeGen.lowercaseFirstLetter;
import static org.scribble.gt.codegen.GenericBehaviour.genGenericBehavior;


public class CodeGen {
    private static final String outputDir = "./generated";
    /**
     * Generates the Erlang code for the given protocol by creating:
     * 1. A generic FSM behavior module.
     * 2. Role-specific callback modules that use the generic FSM behavior.
     */
    public static void genErl(String protocolName, Map<Role, GTLConfig> locals, Set<Op> committing) {
        String erlExtension = ".erl";
        String headerExtension = ".hrl";

        // Now, generate the callback modules for each role
        for (Map.Entry<Role, GTLConfig> entry : locals.entrySet()) {
            Role role = entry.getKey();
            Theta theta = entry.getValue().theta;
            Sigma sigma = entry.getValue().sigma;
            GTLType localType = entry.getValue().type;
            StateM stateM = StateM.translate(localType, role, committing, null, 1).rename();

            String genericModuleName = "gen_" + lowercaseFirstLetter(role.toString());
            StringBuilder genericErlCode = new StringBuilder();

            String lowercaseRole = lowercaseFirstLetter(role.toString());
            StringBuilder cbackErlCode = new StringBuilder();

            StringBuilder headerErlCode = new StringBuilder();
            // Generate the role-specific callback module
            genGenericBehavior(role.toString(), genericErlCode, stateM);
            createErlangFile(protocolName, genericModuleName, genericErlCode, erlExtension);
            //Generate the role-specific callback module
            genCallbackModule(role.toString(), cbackErlCode, stateM);
            createErlangFile(protocolName, lowercaseRole, cbackErlCode, erlExtension);
            // Generate hrl file
            genHeaderFile(headerErlCode, theta, sigma);
            createErlangFile(protocolName, lowercaseRole, headerErlCode, headerExtension);

        }
    }

    private static void genHeaderFile(StringBuilder headerErlCode, Theta theta, Sigma sigma) {
        // pids of the roles in the protocol
        // mc_counters
        headerErlCode.append("-record(state_data, {");
        for (Role role : sigma.map.keySet()) {
            headerErlCode.append(lowercaseFirstLetter(role.toString())).append("_pid :: pid() | undefined, ");
        }
        for (int i = 1; i <= theta.map.size(); i++) {
            headerErlCode.append("mc_counter_").append(i).append(" = 0 :: integer(), ");
        }
        headerErlCode.replace(headerErlCode.length() - 2, headerErlCode.length(), "}).\n\n");
    }


    /**
     * Helper function to create an Erlang file with the generated content.
     */
    private static void createErlangFile(String protocolName, String moduleName, StringBuilder erlRole, String extension) {
        String fileName = moduleName + extension;
        // Create the file with the provided content (implementation depends on the rest of the system)
        // Ensure the output directory exists
        String outputDirectory = outputDir + File.separator + protocolName;
        File directory = new File(outputDirectory);
        if (!directory.exists()) {
            directory.mkdirs();
        }

        // Create a FileWriter for the Erlang file
        try (FileWriter fileWriter = new FileWriter(outputDirectory + File.separator + fileName)) {
            fileWriter.write(String.valueOf(erlRole));
            fileWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        };
    }


}

