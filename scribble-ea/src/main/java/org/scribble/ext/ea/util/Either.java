package org.scribble.ext.ea.util;

import org.jetbrains.annotations.NotNull;

import java.util.Optional;
import java.util.function.Function;

// L=error, R=result
public interface Either<L, R> {

    static <L, R> Either<L, R> left(@NotNull L left) {
        return new Left<>(left);
    }

    static <L, R> Either<L, R> right(@NotNull R right) {
        return new Right<>(right);
    }

    default <T, U> Either<T, U> map(
            Function<? super L, ? extends T> left, Function<? super R, ? extends U> right) {
        //return isLeft() ? new Left<>(left.apply(getLeft().get())) : new Right<>(right.apply(getRight().get()));
        return isLeft() ? mapLeft(left) : mapRight(right);
    }

    // "ifNotPresent"
    default <T, U> Either<T, U> mapLeft(Function<? super L, ? extends T> left) {
        //return map(left, x -> x);
        return new Left<>(left.apply(getLeft().get()));
    }

    // ifPresent
    default <T, U> Either<T, U> mapRight(Function<? super R, ? extends U> right) {
        //return map(x -> x, right);
        return new Right<>(right.apply(getRight().get()));
    }

    default <T, U> Either<T, U> flatMap(
            Function<? super L, ? extends Either<T, U>> left,
            Function<? super R, ? extends Either<T, U>> right) {
        //return isLeft() ? left.apply(getLeft().get()) : right.apply(getRight().get());
        return isLeft() ? flatMapLeft(left) : flatMapRight(right);
        //return flatMapLeft(left).flatMapRight(right);
    }

    // orElse
    default <T, U> Either<T, U> flatMapLeft(
            Function<? super L, ? extends Either<T, U>> left) {
        //return flatMap(left, x -> Either.right(x));
        return left.apply(getLeft().get());
    }

    // andThen
    default <T, U> Either<T, U> flatMapRight(
            Function<? super R, ? extends Either<T, U>> right) {
        //return flatMap(x -> Either.left(x), right);
        return right.apply(getRight().get());
    }

    boolean isLeft();

    boolean isRight();

    Optional<L> getLeft();

    Optional<R> getRight();

    /*static <L, R> Either<L, R> join(Either<L, Either<L, R>> outer) {
        return outer.isLeft()
                ? new Left<>(outer.getLeft().get())
                : outer.mapRight(x -> x.getRight().get());
    }*/
}

class Left<L, R> implements Either<L, R> {

    @NotNull private L left;

    protected Left(@NotNull L left) {
        this.left = left;
    }

    /*@Override
    public <T> Either<T, R> mapLeft(Function<? super L, ? extends T> left) {
        return new Left(left.apply(this.left));
    }

    @Override
    public <T> Either<L, T> mapRight(Function<? super R, ? extends T> left) {
        return new Left<>(this.left);
    }*/

    @Override
    public boolean isLeft() {
        return true;
    }

    @Override
    public boolean isRight() {
        return false;
    }

    @Override
    public Optional<L> getLeft() {
        return Optional.of(this.left);
    }

    @Override
    public Optional<R> getRight() {
        return Optional.empty();
    }
}

class Right<L, R> implements Either<L, R> {

    @NotNull private R right;

    protected Right(@NotNull R right) {
        this.right = right;
    }

    /*@Override
    public <T> Either<T, R> mapLeft(Function<? super L, ? extends T> right) {
        return new Right<>(this.right);
    }

    @Override
    public <T> Either<L, T> mapRight(Function<? super R, ? extends T> right) {
        return new Right<>(right.apply(this.right));
    }*/

    @Override
    public boolean isLeft() {
        return false;
    }

    @Override
    public boolean isRight() {
        return true;
    }

    @Override
    public Optional<L> getLeft() {
        return Optional.empty();
    }

    @Override
    public Optional<R> getRight() {
        return Optional.of(this.right);
    }
}
