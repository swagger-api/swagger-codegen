package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.Parser;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.cache.TemplateCache;
import com.github.jknack.handlebars.io.TemplateSource;

import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

// Replacement for jknack's ConcurrentMapTemplateCache that is safe to use with
// standalone-partial indentation (prettyPrint(true)).
//
// Why this exists:
// jknack's Partial.merge() wraps the partial's TemplateSource via an anonymous class whose
// equals()/hashCode() delegate ONLY to the underlying source (filename + lastModified).
// The wrapper's content(), however, is the partial body re-indented by the include site's
// leading whitespace. Result: the same partial included at two different indents collides
// on a cache lookup, and the first-compiled indent wins for every subsequent include. That
// is observable as silently shifted whitespace in generated output when the same partial
// is included at multiple indent levels.
//
// Fix: key the cache on (filename + content), so two wrappers with the same filename but
// different applied indent get separate entries. Compilation still happens at most once
// per unique (template, indent) pair, which preserves the bulk of the speed-up the cache
// is here for in the first place.
public class IndentAwareTemplateCache implements TemplateCache {

    private final ConcurrentMap<Key, Template> cache = new ConcurrentHashMap<>();
    private boolean reload;

    @Override
    public void clear() {
        cache.clear();
    }

    @Override
    public void evict(TemplateSource source) {
        try {
            cache.remove(keyFor(source));
        } catch (IOException ignored) {
            // best-effort eviction
        }
    }

    @Override
    public Template get(TemplateSource source, Parser parser) throws IOException {
        final Key key = keyFor(source);
        Template cached = cache.get(key);
        if (cached != null && !reload) {
            return cached;
        }
        final Template compiled = parser.parse(source);
        Template previous = cache.putIfAbsent(key, compiled);
        return previous != null ? previous : compiled;
    }

    @Override
    public TemplateCache setReload(boolean reload) {
        this.reload = reload;
        return this;
    }

    private static Key keyFor(TemplateSource source) throws IOException {
        return new Key(source.filename(), source.content(java.nio.charset.StandardCharsets.UTF_8));
    }

    private static final class Key {
        private final String filename;
        private final String content;
        private final int hash;

        Key(String filename, String content) {
            this.filename = filename;
            this.content = content;
            this.hash = 31 * (filename == null ? 0 : filename.hashCode())
                + (content == null ? 0 : content.hashCode());
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Key)) return false;
            Key other = (Key) o;
            return hash == other.hash
                && java.util.Objects.equals(filename, other.filename)
                && java.util.Objects.equals(content, other.content);
        }

        @Override
        public int hashCode() {
            return hash;
        }
    }
}
