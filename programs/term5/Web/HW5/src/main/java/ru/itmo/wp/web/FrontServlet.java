package ru.itmo.wp.web;

import freemarker.template.*;
import ru.itmo.wp.web.exception.NotFoundException;
import ru.itmo.wp.web.exception.RedirectException;
import ru.itmo.wp.web.page.IndexPage;
import ru.itmo.wp.web.page.NotFoundPage;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class FrontServlet extends HttpServlet {
    private static final String BASE_PACKAGE = FrontServlet.class.getPackage().getName() + ".page";
    private static final String DEFAULT_ACTION = "action";
    private static final String DEFAULT_LANGUAGE = "en";

    private Configuration sourceConfiguration;
    private Configuration targetConfiguration;

    private Configuration newFreemarkerConfiguration(String templateDirName, boolean debug)
            throws ServletException {
        File templateDir = new File(templateDirName);
        if (!templateDir.isDirectory()) {
            return null;
        }

        Configuration configuration = new Configuration(Configuration.VERSION_2_3_31);
        try {
            configuration.setDirectoryForTemplateLoading(templateDir);
        } catch (IOException e) {
            throw new ServletException("Can't create freemarker configuration [templateDir="
                    + templateDir + "]");
        }
        configuration.setDefaultEncoding(StandardCharsets.UTF_8.name());
        configuration.setTemplateExceptionHandler(debug ? TemplateExceptionHandler.HTML_DEBUG_HANDLER :
                TemplateExceptionHandler.RETHROW_HANDLER);
        configuration.setLogTemplateExceptions(false);
        configuration.setWrapUncheckedExceptions(true);

        return configuration;
    }

    @Override
    public void init() throws ServletException {
        sourceConfiguration = newFreemarkerConfiguration(
                getServletContext().getRealPath("/") + "../../src/main/webapp/WEB-INF/templates", true);
        targetConfiguration = newFreemarkerConfiguration(
                getServletContext().getRealPath("WEB-INF/templates"), false);

        sourceConfiguration.setLocalizedLookup(false);
        targetConfiguration.setLocalizedLookup(false);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        process(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        process(request, response);
    }

    private void process(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        setLanguage(request);
        Route route = Route.newRoute(request);
        try {
            process(route, request, response);
        } catch (NotFoundException e) {
            try {
                process(Route.newNotFoundRoute(), request, response);
            } catch (NotFoundException notFoundException) {
                throw new ServletException(notFoundException);
            }
        }
    }

    private void setLanguage(HttpServletRequest request) {
        String lang = request.getParameter("lang");
        if (lang != null && lang.matches("[a-z]{2}")) {
            request.getSession().setAttribute("lang", lang);
        }
    }

    private void process(Route route, HttpServletRequest request, HttpServletResponse response)
            throws NotFoundException, ServletException, IOException {
        Class<?> pageClass;
        try {
            pageClass = Class.forName(route.getClassName());
        } catch (ClassNotFoundException e) {
            throw new NotFoundException();
        }

        Method method = getMethod(route, pageClass);

        Object page;
        try {
            page = pageClass.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException
                 | NoSuchMethodException | InvocationTargetException e) {
            throw new ServletException("Can't create page [pageClass="
                    + pageClass + "]");
        }

        Map<String, Object> view = new HashMap<>();
        method.setAccessible(true);
        try {
            //method.invoke(page, request, view);

            Object[] args = new Object[method.getParameterCount()];
            Class<?>[] parameterTypes = method.getParameterTypes();

            for (int i = 0; i < parameterTypes.length; i++) {
                if (parameterTypes[i].equals(HttpServletRequest.class)) {
                    args[i] = request;
                } else if (parameterTypes[i].equals(Map.class)) {
                    args[i] = view;
                }
            }

            method.invoke(page, args);

        } catch (IllegalAccessException e) {
            throw new ServletException("Can't invoke action method [pageClass="
                    + pageClass + ", method=" + method + "]");
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof RedirectException) {
                RedirectException redirectException = (RedirectException) cause;
                response.sendRedirect(redirectException.getTarget());
                return;
            } else {
                throw new ServletException("Can't invoke action method [pageClass="
                        + pageClass + ", method=" + method + "]", cause);
            }
        }

        Template template = newTemplate(pageClass.getSimpleName() + ".ftlh", request.getSession());
        response.setContentType("text/html");
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        try {
            System.out.println(template.getName());
            template.process(view, response.getWriter());
        } catch (TemplateException e) {
            if (sourceConfiguration == null) {
                throw new ServletException("Can't render template [pageClass="
                        + pageClass + ", action=" + method + "]", e);
            }
        }
    }

    private static Method getMethod(Route route, Class<?> pageClass) throws NotFoundException {
        Method method = null;
        for (Class<?> clazz = pageClass; method == null && clazz != null; clazz = clazz.getSuperclass()) {
            //method = clazz.getDeclaredMethod(route.getAction(), HttpServletRequest.class, Map.class);
            Method[] methods = clazz.getDeclaredMethods();
            for (Method m : methods) {
                if (m.getName().equals(route.getAction())) {
                    method = m;
                    break;
                }
            }
        }

        if (method == null) {
            throw new NotFoundException();
        }

        return method;
    }

    private Template newTemplate(String templateName, HttpSession session) throws ServletException {
        Template template;

        String lang = (String) session.getAttribute("lang");
        if (lang == null || lang.isEmpty()) {
            lang = DEFAULT_LANGUAGE;
        }

        String localizedTemplateName = templateName.substring(0, templateName.lastIndexOf('.'))
                + (lang.equals(DEFAULT_LANGUAGE) ? "" : "_" + lang) + ".ftlh";
        template = getTemplate(localizedTemplateName);

        if (template == null) {
            template = getTemplate(templateName);
        }

//        if (sourceConfiguration != null) {
//            try {
//                template = sourceConfiguration.getTemplate(templateName);
//            } catch (TemplateNotFoundException ignored) {
//                // No operations.
//            } catch (IOException e) {
//                throw new ServletException("Can't load template [templateName=" + templateName + "]", e);
//            }
//        }
//
//        if (template == null && targetConfiguration != null) {
//            try {
//                template = targetConfiguration.getTemplate(templateName);
//            } catch (TemplateNotFoundException ignored) {
//                // No operations.
//            } catch (IOException e) {
//                throw new ServletException("Can't load template [templateName=" + templateName + "]", e);
//            }
//        }

        if (template == null) {
            throw new ServletException("Can't find template [templateName=" + templateName + "]");
        }

        return template;
    }

    private Template getTemplate(String templateName) throws ServletException {
        Template template = null;
        if (sourceConfiguration != null) {
            template = getTemplate(templateName, sourceConfiguration);
        }

        if (targetConfiguration != null && template == null) {
            template = getTemplate(templateName, targetConfiguration);
        }

        return template;
    }


    private Template getTemplate(String templateName, Configuration configuration) throws ServletException {
        try {
            return configuration.getTemplate(templateName);
        } catch (TemplateNotFoundException ignored) {
            // No operations.
        } catch (IOException e) {
            throw new ServletException("Can't load template [templateName=" + templateName + "]", e);
        }

        return null;
    }

    private static class Route {
        private final String className;
        private final String action;

        private Route(String className, String action) {
            this.className = className;
            this.action = action;
        }

        private String getClassName() {
            return className;
        }

        private String getAction() {
            return action;
        }

        private static Route newNotFoundRoute() {
            return new Route(NotFoundPage.class.getName(), DEFAULT_ACTION);
        }

        private static Route newIndexRoute() {
            return new Route(IndexPage.class.getName(), DEFAULT_ACTION);
        }

        private static Route newRoute(HttpServletRequest request) {
            String uri = request.getRequestURI();

            List<String> classNameParts = Arrays.stream(uri.split("/"))
                    .filter(part -> !part.isEmpty())
                    .collect(Collectors.toList());

            if (classNameParts.isEmpty()) {
                return newIndexRoute();
            }

            StringBuilder simpleClassName = new StringBuilder(classNameParts.get(classNameParts.size() - 1));
            int lastDotIndex = simpleClassName.lastIndexOf(".");
            simpleClassName.setCharAt(lastDotIndex + 1,
                    Character.toUpperCase(simpleClassName.charAt(lastDotIndex + 1)));
            classNameParts.set(classNameParts.size() - 1, simpleClassName.toString());

            String className = BASE_PACKAGE + "." + String.join(".", classNameParts) + "Page";

            String action = request.getParameter("action");
            if (action == null || action.isEmpty()) {
                action = DEFAULT_ACTION;
            }

            return new Route(className, action);
        }
    }
}
