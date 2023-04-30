package org.scribble.ext.ea.util;

import org.jetbrains.annotations.NotNull;

import java.util.Optional;
import java.util.function.Function;

// L=error, R=result
public interface Either<L, R> {

    static <L, R> Either<L, R> left(@NotNull L left) {
        return new Left<>(left);
    }

    static <L, R> Either<L, R> right(R right) {  // Allow null for now, Either<L, Void>
        return new Right<>(right);
    }

    /*default <T, U> Either<T, U> map(
            Function<? super L, ? extends T> left,
            Function<? super R, ? extends U> right) {
        return isLeft() ? new Left<>(left.apply(getLeft().get())) : new Right(right.apply(getRight().get()));
    }*/

    // mapLeft -- isLeft ? map Left : unchanged Right

    // ifPresent
    default <U> Either<L, U> mapRight(Function<? super R, ? extends U> right) {
        return isRight() ? new Right<>(right.apply(getRight().get())) : new Left<>(getLeft().get());
        //return map(x -> x, right);
    }

    /*default <T, U> Either<T, U> flatMap(
            Function<? super L, ? extends Either<T, U>> left,
            Function<? super R, ? extends Either<T, U>> right) {
        return isLeft() ? left.apply(getLeft().get()) : right.apply(getRight().get());
    }*/

    // flatMapLeft -- isLeft ? flatMap Left : unchanged Right

    // andThen
    default <U> Either<L, U> flatMapRight(Function<? super R, ? extends Either<L, U>> right) {
        return isRight() ? right.apply(getRight().get()) : new Left<>(getLeft().get());  // "join" inlined
        //return flatMap(x -> Either.left(x), right);
    }

    boolean isLeft();

    boolean isRight();

    Optional<L> getLeft();

    Optional<R> getRight();

    /*static <L, R> Either<L, R> join(Either<L, Either<L, R>> outer) {
        return outer.isLeft()
                ? new Left<>(outer.getLeft().get())
                : outer.getRight().get();  //outer.mapRight(x -> x.getRight().get());
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

    @Override
    public String toString() {
        return "Right(" + this.right + ")";
    }
}
