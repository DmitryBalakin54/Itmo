package info.kgeorgiy.ja.balakin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.concurrent.*;
import java.util.*;

/**
 * This class implements {@link NewCrawler}, this is a  web crawler implementation that can download web pages up to
 * a specified depth.
 * It uses a multi-thread approach for recursive downloading links from web pages.
 *
 * @author DmitryBalakin
 */
public class WebCrawler implements NewCrawler {
    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;

    /**
     * method for running this class from the command line:
     * WebCrawler url [depth [downloads [extractors [perHost]]]].
     *
     * @param args command-line arguments: String url, int depth, int downloaders, int extractors, int perHost(ignored)
     */
    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Arguments is null");
            return;
        }

        if (args.length != 5) {
            System.err.println("Arguments length must be 5");
            return;
        }

        try (var webCrawler = new WebCrawler(new CachingDownloader(1.0),
                Integer.parseInt(args[2]),
                Integer.parseInt(args[3]),
                Integer.parseInt(args[4]))) {

            var res = webCrawler.download(args[0], Integer.parseInt(args[1]));
            webCrawler.close();
            System.out.println(res.getDownloaded());
            System.out.println(res.getErrors());
        } catch (IOException e) {
            System.err.printf("Cannot create CachingDownloader: %s%n", e.getMessage());
        } catch (NumberFormatException e) {
            System.err.printf("Arguments from 2 to 5 must be int: %s%n", e.getMessage());
        }
    }

    /**
     * Constructs a new WebCrawler instance.
     *
     * @param downloader the downloader implementation to use for downloading web pages
     * @param downloaders the number of downloader threads to use
     * @param extractors the number of extractor threads to use
     * @param ignoredPerHost maximum number of downloads per host(ignored)
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int ignoredPerHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
    }

    /**
     * Recursively loads web pages starting from a specified URL to a specified depth.
     *
     * @param url the URL to start crawling from
     * @param depth the maximum depth to crawl to
     * @param excludes URLs containing one of given substrings are ignored.
     * @return the result containing downloaded URLs and errors encountered
     */
    @Override
    public Result download(String url, int depth, Set<String> excludes) {
        DownloaderData data = new DownloaderData(url);
        Phaser phaser = new Phaser(1);

        for (int i = 1; i <= depth; i++) {
            int ind = i;
            data.getLastUrls().forEach(curUrl -> downloadUrl(curUrl, data, phaser, ind == depth, excludes));
            phaser.arriveAndAwaitAdvance();
            data.updateUrls();
        }

        phaser.arriveAndDeregister();

        return data.getResult();
    }


    private void downloadUrl(String url, DownloaderData data, Phaser phaser, boolean isLast, Set<String> excludes) {
        if (data.isCached(url) || excludes.stream().anyMatch(url::contains)) {
            return;
        }

        data.download(url);

        phaser.register();
        downloaders.submit(() -> {
            try {
                var document = downloader.download(url);
                if (!isLast) {
                    getUrls(document, data, phaser);
                }
            } catch (IOException e) {
                data.addError(url, e);
            } finally {
                phaser.arriveAndDeregister();
            }
        });
    }

    private void getUrls(Document document, DownloaderData data, Phaser phaser) {
        phaser.register();
        extractors.submit(() -> {
           try {
                data.addNewLinks(document.extractLinks());
           } catch (IOException ignored) {
           } finally {
               phaser.arriveAndDeregister();
           }
        });
    }

    /**
     * Closes the web crawler, shutting down all associated threads.
     */
    @Override
    public void close() {
        downloaders.shutdown();
        extractors.shutdown();
    }

    private static class DownloaderData {
        private final Set<String> downloadedUrls;
        private final Map<String, IOException> errors;
        private final Queue<String> currentUrls;
        private final Queue<String> lastUrls;

        public DownloaderData() {
            this.downloadedUrls = ConcurrentHashMap.newKeySet();
            this.errors = new ConcurrentHashMap<>();
            this.currentUrls = new ConcurrentLinkedQueue<>();
            this.lastUrls = new ConcurrentLinkedQueue<>();
        }

        public DownloaderData(String url) {
            this();
            lastUrls.add(url);
        }

        public Queue<String> getLastUrls() {
            return lastUrls;
        }

        public void addError(String url, IOException error) {
            downloadedUrls.remove(url);
            errors.put(url, error);
        }

        public void addNewLinks(List<String> newLinks) {
            currentUrls.addAll(newLinks);
        }
        public void download(String url) {
            downloadedUrls.add(url);
        }

        public boolean isCached(String url) {
            return downloadedUrls.contains(url) || errors.containsKey(url);
        }
        private void updateUrls() {
            lastUrls.clear();
            lastUrls.addAll(currentUrls);
            currentUrls.clear();
        }
        public Result getResult() {
            return new Result(downloadedUrls.stream().toList(), errors);
        }
    }
}
