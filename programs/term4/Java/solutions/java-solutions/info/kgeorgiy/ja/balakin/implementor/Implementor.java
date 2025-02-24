package info.kgeorgiy.ja.balakin.implementor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

import javax.tools.ToolProvider;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

/**
 * This class provides implementation for the {@link Impler} and {@link JarImpler} interfaces.
 * It allows generating implementations for interfaces and packing them into a JAR file.
 *
 * @author DmitryBalakin
 */
public class Implementor implements Impler, JarImpler {

    /**
     * Empty constructor for {@link Implementor}
     */
    public Implementor() {

    }

    /**
     * String that has the value "Impl" - suffix that is added to the interface during implementation
     */
    private static final String CLASS_NAME_SUFFIX = "Impl";

    /**
     * String that has the value ".java"
     */
    private static final String JAVA_SUFFIX = ".java";

    /**
     * String that has the value ".class"
     */
    private static final String CLASS_SUFFIX = ".class";

    /**
     * String that has the value "1.0" - manifest version
     */
    private static final String MANIFEST_VERSION = "1.0";

    /**
     * Main method to run the Implementor class.
     * It expects command line arguments specifying either:
     * <ul>
     *     <li>The fully qualified name of the interface to implement.</li>
     *     <li>The option "-jar" followed by the fully qualified name of the interface to implement and the path to
     *     save the JAR file.</li>
     * </ul>
     *
     * @param args Array containing {@code [class-name]} or {@code [-jar, class-name, path-to-jar.jar]}.
     */
    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Args is null");
            return;
        }

        var implementor = new Implementor();
        try {
            if (args.length == 1 && args[0] != null) {
                implementor.implement(Class.forName(args[0]), Path.of("./impl"));
            } else if (args.length == 3 && args[0].equals("-jar") && args[1] != null && args[2] != null) {
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                System.err.println("Incorrect args");
            }
        } catch (ClassNotFoundException e) {
            System.out.println("Class not found " + args[0]);
        } catch (ImplerException e) {
            System.out.println("Error implementing interface: " + e.getMessage());
        }

    }


    /**
     * Implements the specified interface by generating a corresponding class file.
     * The implementation class will contain default implementations for all abstract methods
     * declared in the interface.
     *
     * @param token the interface token to implement.
     * @param root  the root directory where the implementation class file will be generated.
     * @throws ImplerException if the specified interface token is null,
     *                         is a private interface, or if the root path is null,
     *                         or if an error occurs during file writing.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (token == null) {
            throw new ImplerException("Token is null!");
        }

        if (!token.isInterface() || (token.getModifiers() & Modifier.PRIVATE) > 0) {
            throw new ImplerException("Token must be a non-private interface!");
        }

        if (root == null) {
            throw new ImplerException("Path is null!");
        }


        try (BufferedWriter writer = Files.newBufferedWriter(makePath(token, root), StandardCharsets.UTF_8)) {
            write(String.format("%s%n%s", getPackage(token), getBody(token)), writer);
        } catch (IOException e) {
            throw new ImplerException("Error writing output file");
        }
    }

    /**
     * Writes the specified string with Unicode escapes to the writer.
     *
     * @param res The string to write.
     * @param wr  The writer to write to.
     * @throws IOException If an I/O error occurs.
     */
    private void write(String res, Writer wr) throws IOException {
        for (var ch : res.toCharArray()) {
            wr.write(String.format("\\u%04X", (int) ch));
        }
    }

    /**
     * Creates the path for the generated Java source file.
     *
     * @param token The interface for which the implementation is generated.
     * @param root  The root directory.
     * @return The path to the generated Java source file.
     * @throws ImplerException If the directory cannot be created.
     */
    private Path makePath(Class<?> token, Path root) throws ImplerException {
        var dir = root.resolve(getPath(token.getPackage().getName(), File.separatorChar));

        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            throw new ImplerException("Cannot make dir " + dir);
        }

        return dir.resolve(token.getSimpleName() + CLASS_NAME_SUFFIX + JAVA_SUFFIX);
    }

    /**
     * Generates the class implementation package-string.
     *
     * @param token The interface to implement.
     * @return The package-string of the implemented class.
     */
    private String getPackage(Class<?> token) {
        if (token.getPackage() == null) {
            return "";
        }

        return String.format("%s;%n", token.getPackage());
    }

    /**
     * Generates the class implementation source code.
     *
     * @param token The interface to implement.
     * @return The source code of the implemented class.
     */
    private String getBody(Class<?> token) {
        return String.format("%s {%n%n%s}%n", getHead(token), getMethods(token));
    }

    /**
     * Generates the head of the implemented class.
     *
     * @param token The interface to implement.
     * @return The head of the implemented class.
     */
    private String getHead(Class<?> token) {
        var parent = token.getName();
        parent = parent.substring(parent.lastIndexOf('.') + 1).replace('$', '.');

        return String.format("public class %s%s implements %s", token.getSimpleName(), CLASS_NAME_SUFFIX, parent);
    }

    /**
     * Generates the methods implementation source code.
     *
     * @param token implementing class.
     * @return The source code of the implemented methods.
     */
    private String getMethods(Class<?> token) {
        return Arrays.stream(token.getMethods())
                .map(method -> String.format("%s%n", getMethod(method)))
                .collect(Collectors.joining(System.lineSeparator()));
    }

    /**
     * Generates the method implementation source code.
     *
     * @param method The method to implement.
     * @return The source code of the implemented method.
     */
    private String getMethod(Method method) {
        return String.format(
                "%s%s %s %s(%s) %s {%n%s%s%n%s}",
                tabulations(1),
                getMethodModifiers(method),
                getMethodType(method),
                method.getName(),
                getMethodArguments(method),
                getMethodExceptions(method),
                tabulations(2),
                getMethodBody(method),
                tabulations(1)
        );
    }

    /**
     * Returns the modifiers string for the given method.
     *
     * @param method The method.
     * @return The modifiers string.
     */
    private String getMethodModifiers(Method method) {
        return Modifier.toString(method.getModifiers() & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT);
    }

    /**
     * Returns the type string for the given method.
     *
     * @param method The method.
     * @return The type string.
     */
    private String getMethodType(Method method) {
        return method.getReturnType().getCanonicalName();
    }

    /**
     * Returns the arguments string for the given method.
     *
     * @param method The method.
     * @return The arguments string.
     */
    private String getMethodArguments(Method method) {
        return Arrays.stream(method.getParameters())
                .map(param -> String.format("%s %s", param.getType().getCanonicalName(), param.getName()))
                .collect(Collectors.joining(", "));
    }

    /**
     * Returns the body of the implemented method.
     *
     * @param method The method.
     * @return The body of the implemented method.
     */
    private String getMethodBody(Method method) {
        if (method.getReturnType().getSimpleName().equals("void")) {
            return "";
        }

        return String.format("return %s;", getDefaultValue(method.getReturnType()));
    }

    /**
     * Returns default value for type.
     *
     * @param type The type of return value.
     * @return The default value of type.
     */
    private String getDefaultValue(Class<?> type) {
        if (type.isPrimitive()) {
            return type.equals(boolean.class) ? "false" : "0";
        }

        return "null";
    }

    /**
     * Returns the exceptions string for the given method.
     *
     * @param method The method.
     * @return The exceptions string.
     */
    private String getMethodExceptions(Method method) {
        var exceptions = Arrays.stream(method.getExceptionTypes())
                .map(Class::getCanonicalName)
                .collect(Collectors.joining(", "));

        if (exceptions.isEmpty()) {
            return "";
        }

        return String.format("throws %s", exceptions);
    }

    /**
     * Returns the tabulation string for the given count.
     *
     * @param count The count of tabulations.
     * @return The tabulation string.
     */
    private String tabulations(int count) {
        return "    ".repeat(Math.max(0, count));
    }

    /**
     * Implements the specified interface and creates a JAR file containing the implementation class.
     *
     * @param token   the interface token to implement.
     * @param jarFile the path to the JAR file to be created.
     * @throws ImplerException if unable to implement the interface, compile the implementation class,
     *                         create the JAR file, or due to security restrictions.
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        try {
            var tmp = Files.createTempDirectory(Path.of("."), ".tmp");
            implement(token, tmp);

            if (compClasses(token, makePath(token, tmp), tmp) != 0) {
                throw new ImplerException("Compiler failed");
            }

            createJar(token, jarFile, tmp);
            tmp.toFile().deleteOnExit();
        } catch (SecurityException | IOException e) {
            throw new ImplerException("Cannot make tmp dir");
        } catch (URISyntaxException e) {
            throw new ImplerException("Cannot make uri");
        }
    }

    /**
     * Returns the path string with separators replaced.
     *
     * @param path      The original path.
     * @param separator The separator character to replace.
     * @return The path string with separators replaced.
     */
    private String getPath(String path, char separator) {
        return path.replace('.', separator);
    }

    /**
     * Creates a JAR file with the implementation of the given interface.
     *
     * @param token   The interface to implement.
     * @param jarFile The JAR file to create.
     * @param tmp     The temporary directory.
     * @throws ImplerException If the JAR file cannot be created.
     */
    private void createJar(Class<?> token, Path jarFile, Path tmp) throws ImplerException {
        var manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, MANIFEST_VERSION);

        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            var className = String.format(
                    "%s%s%s%s%s",
                    getPath(token.getPackageName(), '/'),
                    (token.getPackageName().isEmpty() ? "" : '/'),
                    token.getSimpleName(),
                    CLASS_NAME_SUFFIX,
                    CLASS_SUFFIX
            );

            jarOutputStream.putNextEntry(new ZipEntry(className));
            Files.copy(tmp.resolve(className), jarOutputStream);
        } catch (IOException e) {
            throw new ImplerException("Error jar generating");
        }
    }

    /**
     * Returns the source path for the given class.
     *
     * @param token The class.
     * @param tmp   The temporary directory.
     * @return The source path for the class.
     * @throws URISyntaxException If the URI syntax is incorrect.
     */
    private Path getCode(Class<?> token, Path tmp) throws URISyntaxException {
        return tmp.resolve(Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()));
    }

    /**
     * Compiles the class.
     *
     * @param token The class.
     * @param path  The path to the class file.
     * @param tmp   The temporary directory.
     * @return The exit code of the compilation process.
     * @throws URISyntaxException If the URI syntax is incorrect.
     * @throws ImplerException    If the Java compiler not found.
     */
    private int compClasses(Class<?> token, Path path, Path tmp) throws URISyntaxException, ImplerException {
        if (ToolProvider.getSystemJavaCompiler() == null) {
            throw new ImplerException("Java compiler not found");
        }

        return ToolProvider.getSystemJavaCompiler().run(null, null, null,
                "-cp", getCode(token, tmp).toString(), path.toString()
        );
    }
}
