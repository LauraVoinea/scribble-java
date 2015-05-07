package org.scribble2.cli;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.scribble2.main.MainContext;
import org.scribble2.model.Module;
import org.scribble2.model.visit.Job;
import org.scribble2.model.visit.JobContext;
import org.scribble2.model.visit.Projector;
import org.scribble2.sesstype.name.ProtocolName;
import org.scribble2.sesstype.name.Role;
import org.scribble2.util.ScribbleException;

public class CommandLine implements Runnable
{
	public static final String PATH_FLAG = "-path";
	public static final String PROJECT_FLAG = "-project";
	
	protected enum Arg { MAIN, PATH, PROJECT }
	
	private final Map<Arg, String[]> args;
	
	public CommandLine(String[] args)
	{
		this.args = new CommandLineArgumentParser(args).getArgs();
		if (!this.args.containsKey(Arg.MAIN))
		{
			throw new RuntimeException("No main module has been specified\r\n");
		}
	}

	public static void main(String[] args)
	{
		new CommandLine(args).run();
	}

	@Override
	public void run()
	{
		//..HERE: import path should be a CL parameter, not MainContext; MainContext should be working off abstract resource locators only -- but then Resource and ResourceLocator should be made abstract from (file)paths (cf. toPath in ScribbleModuleLoader)
		//..HERE: use Path API; Job takes MainContext as argument
		//..HERE: fix CliJob/Job factoring (e.g. pointers and Modules -- maybe already ok); CliJob should record job parameters; smoothen related APIs; additional flags, e.g. projection
			
		MainContext mc = newMainContext();
		Job job = newJob(mc);

		try
		{
			job.checkWellFormedness();

			JobContext jc = job.getContext();
			if (this.args.containsKey(Arg.PROJECT))
			{
				outputProjection(jc);
			}
		}
		catch (ScribbleException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	private void outputProjection(JobContext jc)
	{
		Map<ProtocolName, Module> projs = jc.getProjections();
		Role role = new Role(this.args.get(Arg.PROJECT)[1]);
		ProtocolName proto = Projector.makeProjectedProtocolNameNode(new ProtocolName(jc.main, this.args.get(Arg.PROJECT)[0]), role).toName();
		if (!projs.containsKey(proto))
		{
			throw new RuntimeException("Bad projection args: " + Arrays.toString(this.args.get(Arg.PROJECT)));
		}
		System.out.println(projs.get(proto));
	}
	
	private MainContext newMainContext()
	{
		Path mainpath = CommandLine.parseMainPath(this.args.get(Arg.MAIN)[0]);
		List<Path> impaths = this.args.containsKey(Arg.PATH) ? CommandLine.parseImportPaths(this.args.get(Arg.PATH)[0]) : Collections.emptyList();
		return new MainContext(impaths, mainpath);
	}
	
	//protected Module loadModule(Resource resource) {
	//protected void loadModules(List<String> path, String mainpath)
	private Job newJob(MainContext mc)
	{
		//Job job = new Job(impaths, mainpath, cjob.getModules(), cjob.getModules().get(cjob.main));
		//Job job = new Job(cjob);  // Doesn't work due to (recursive) maven dependencies
		return new Job(mc.getModules(), mc.main);
	}
	
	private static Path parseMainPath(String path)
	{
		return Paths.get(path);
	}
	
	private static List<Path> parseImportPaths(String paths)
	{
		return Arrays.stream(paths.split(File.pathSeparator)).map((s) -> Paths.get(s)).collect(Collectors.toList());
	}

	/*/**
	 * This method projects the supplied module.
	 * 
	 * @param module
	 *            The module
	 * @param resource
	 *            The resource
	 * /
	protected void project(Module module, Resource resource) {
		String resourceRoot = _locator.getResourceRoot(resource);

		if (resourceRoot == null) {
			System.err.println("Unable to find root location for resource");
			return;
		}

		ModuleContext context = new DefaultModuleContext(resource, module,
				_loader);

		java.util.Set<Module> modules = PROJECTOR.project(context, module,
				LOGGER);

		for (Module m : modules) {
			String name = m.getName().replace('.', java.io.File.separatorChar);

			String path = resourceRoot + java.io.File.separatorChar + name
					+ ".scr";

			try {
				java.io.FileOutputStream fos = new java.io.FileOutputStream(
						path);

				fos.write(m.toString().getBytes());

				fos.flush();
				fos.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * This method validates the trace location. The location can either be a
	 * single trace file, or a folder containing one or more trace files.
	 * 
	 * @param location
	 *            The location
	 * @return Whether the location is valid
	 * /
	protected static boolean validateTraceLocation(java.io.File location) {
		boolean ret = false;

		if (location.exists()) {
			if (location.isFile()) {
				ret = location.getName().endsWith(".trace");

			} else if (location.isDirectory()) {
				for (java.io.File child : location.listFiles()) {
					if (validateTraceLocation(child)) {
						ret = true;
						break;
					}
				}
			}
		}

		return (ret);
	}

	/**
	 * This method recursively scans the supplied location to determine if a
	 * trace file is present, and if found, simulates it.
	 * 
	 * @param location
	 *            The location
	 * @return Whether simulation was successful
	 * /
	protected boolean simulate(java.io.File location) {
		boolean ret = true;

		if (location.exists()) {
			if (location.isFile()) {

				if (location.getName().endsWith(".trace")) {
					System.out.println("\r\nSimulate: " + location.getPath());

					try {
						java.io.InputStream is = new java.io.FileInputStream(
								location);

						Trace trace = MAPPER.readValue(is, Trace.class);

						is.close();

						SimulatorContext context = new DefaultSimulatorContext(
								_locator);

						Simulator simulator = new Simulator();

						final java.util.List<Step> failed = new java.util.ArrayList<Step>();

						SimulationListener l = new SimulationListener() {

							public void start(Trace trace) {
							}

							public void start(Trace trace, Step step) {
							}

							public void successful(Trace trace, Step step) {
								System.out.println("\tSUCCESSFUL: " + step);
							}

							public void failed(Trace trace, Step step) {
								System.out.println("\tFAILED: " + step);
								failed.add(step);
							}

							public void stop(Trace trace) {
							}

						};

						simulator.addSimulationListener(l);

						simulator.simulate(context, trace);

						simulator.removeSimulationListener(l);

						if (failed.size() > 0) {
							ret = false;
						}
					} catch (Exception e) {
						e.printStackTrace();
						ret = false;

					}
				}

			} else if (location.isDirectory()) {
				for (java.io.File child : location.listFiles()) {
					ret = simulate(child);

					if (!ret) {
						break;
					}
				}
			}
		}

		return (ret);
	}*/
}

class CommandLineArgumentParser
{
	private final Map<String, CommandLine.Arg> FLAGS = new HashMap<>();

	private final String[] args;
	private Map<CommandLine.Arg, String[]> parsed = new HashMap<>();
	
	public CommandLineArgumentParser(String[] args)
	{
		this.args = args;

		addFlags();
		parseArgs();
	}		
	
	private void addFlags()
	{
		this.FLAGS.put(CommandLine.PATH_FLAG, CommandLine.Arg.PATH);
		this.FLAGS.put(CommandLine.PROJECT_FLAG, CommandLine.Arg.PROJECT);
	}
	
	private void parseArgs()
	{
		for (int i = 0; i < this.args.length; i++)
		{
			String arg = args[i];
			if (this.FLAGS.containsKey(arg))
			{
				i = this.parseFlag(i);
			}
			else
			{
				if (this.parsed.containsKey(CommandLine.Arg.MAIN))
				{
					throw new RuntimeException("Bad: " + arg);
				}
				parseMain(i);
			}
		}
	}

	// Pre: i is the index of the current flag to parse
	// Post: i is the index of the last argument parsed
	private int parseFlag(int i)
	{
		String flag = this.args[i];
		switch (flag)
		{
			case CommandLine.PATH_FLAG:
			{
				return parsePath(i);
			}
			case CommandLine.PROJECT_FLAG:
			{
				return parseProject(i);
			}
			default:
			{
				throw new RuntimeException(flag);
			}
		}
	}

	private void parseMain(int i)
	{
		String main = args[i];
		if (!CommandLineArgumentParser.validateModuleName(main))
		{
			throw new RuntimeException("Bad: " + main);
		}
		this.parsed.put(CommandLine.Arg.MAIN, new String[] { main } );
	}

	private int parsePath(int i)
	{
		if ((i + 1) >= this.args.length)
		{
			throw new RuntimeException("Missing path argument");
		}
		String path = this.args[++i];
		if (!validatePaths(path))
		{
			throw new RuntimeException("Module path '"+ path +"' is not valid\r\n");
		}
		this.parsed.put(this.FLAGS.get(CommandLine.PATH_FLAG), new String[] { path });
		return i;
	}
	
	private int parseProject(int i)
	{
		if ((i + 2) >= this.args.length)
		{
			throw new RuntimeException("Missing protocol/role arguments");
		}
		String proto = this.args[++i];
		String role = this.args[++i];
		/*if (!validateProtocolName(proto))  // TODO
		{
			throw new RuntimeException("Protocol name '"+ proto +"' is not valid\r\n");
		}*/
		this.parsed.put(this.FLAGS.get(CommandLine.PROJECT_FLAG), new String[] { proto, role } );
		return i;
	}

	private static boolean validateModuleName(String module)
	{
		for (String part : module.split("."))
		{
			for (int i = 0; i < part.length(); i++)
			{
				if (!Character.isLetterOrDigit(part.charAt(i)))
				{
					if (part.charAt(i) != '_')
					{
						return false;
					}
				}
			}
		}
		return true;
	}

	private static boolean validatePaths(String paths)
	{
		for (String path : paths.split(":"))
		{
			if (!new File(path).isDirectory())
			{
				return false;
			}
		}
		return true;
	}
	
	public Map<CommandLine.Arg, String[]> getArgs()
	{
		return this.parsed;
	}
}
