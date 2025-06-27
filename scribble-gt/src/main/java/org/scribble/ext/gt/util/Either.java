package org.scribble.ext.gt.util;

import org.jetbrains.annotations.NotNull;

import java.util.function.Function;

// L=error, R=result
public interface Either<L, R> {

    static <L, R> Either<L, R> left(@NotNull L left) {
        return new Left<>(left);
    }

    static <L, R> Either<L, R> right(R right) {  // Allow null for now, Either<L, Void>
        return new Right<>(right);
    }

    // ifPresent
    default <U> Either<L, U> mapRight(Function<? super R, ? extends U> right) {
        return isRight() ? new Right<>(right.apply(getRight())) : new Left<>(getLeft());
    }

    // andThen
    default <U> Either<L, U> flatMapRight(Function<? super R, ? extends Either<L, U>> right) {
        return isRight() ? right.apply(getRight()) : new Left<>(getLeft());  // "join" inlined
        //return flatMap(x -> Either.left(x), right);
    }

    boolean isLeft();

    boolean isRight();

    L getLeft();

    R getRight();
}

class Left<L, R> implements Either<L, R> {

    @NotNull private L left;

    protected Left(@NotNull L left) {
        this.left = left;
    }

    @Override
    public boolean isLeft() {
        return true;
    }

    @Override
    public boolean isRight() {
        return false;
    }

    @Override
    public L getLeft() {
        return this.left;
    }

    @Override
    public R getRight() {
        throw new RuntimeException("Not Right: " + this);
    }

    @Override
    public String toString() {
        return "Left(" + this.left + ")";
    }
}

class Right<L, R> implements Either<L, R> {

    private R right;  // Allow null for now, Either<L, Void>

    protected Right(R right) {
        this.right = right;
    }

    @Override
    public boolean isLeft() {
        return false;
    }

    @Override
    public boolean isRight() {
        return true;
    }

    @Override
    public L getLeft() {
        throw new RuntimeException("Not Left: " + this);
    }

    @Override
    public R getRight() {
        return this.right;
    }

    @Override
    public String toString() {
        return "Right(" + this.right + ")";
    }
}
