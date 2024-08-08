package org.scribble.ext.gt.runtime;

public class GTMixedException extends Exception {
    public GTMixedException() {
    }

    public GTMixedException(String message) {
        super(message);
    }

    public GTMixedException(String message, Throwable cause) {
        super(message, cause);
    }

    public GTMixedException(Throwable cause) {
        super(cause);
    }

    public GTMixedException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
