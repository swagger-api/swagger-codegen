package io.swagger.client;

import java.util.concurrent.CompletionStage;
import retrofit2.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Creates {@link CallAdapter} instances that convert {@link Call} into {@link java.util.concurrent.CompletionStage}
 */
public class Play25CallAdapterFactory extends CallAdapter.Factory {

    @Override
    public CallAdapter<?, ?> get(Type returnType, Annotation[] annotations, Retrofit retrofit) {
        if (!(returnType instanceof ParameterizedType)) {
            return null;
        }

        ParameterizedType type = (ParameterizedType) returnType;
        if (type.getRawType() != CompletionStage.class) {
            return null;
        }

        return createAdapter((ParameterizedType) returnType);
    }

    private Type getTypeParam(ParameterizedType type) {
        Type[] types = type.getActualTypeArguments();
        if (types.length != 1) {
            throw new IllegalStateException("Must be exactly one type parameter");
        }

        Type paramType = types[0];
        if (paramType instanceof WildcardType) {
            return ((WildcardType) paramType).getUpperBounds()[0];
        }

        return paramType;
    }

    private CallAdapter<?, CompletionStage<?>> createAdapter(ParameterizedType returnType) {
        Type parameterType = getTypeParam(returnType);
        return new ValueAdapter(parameterType);
    }

    /**
     * Adpater that coverts values returned by API interface into CompletionStage
     */
    static final class ValueAdapter<R> implements CallAdapter<R, CompletionStage<R>> {

        private final Type responseType;

        ValueAdapter(Type responseType) {
            this.responseType = responseType;
        }

        @Override
        public Type responseType() {
            return responseType;
        }

        @Override
        public CompletionStage<R> adapt(final Call<R> call) {
            final CompletableFuture<R> promise = new CompletableFuture();

            call.enqueue(new Callback<R>() {

                @Override
                public void onResponse(Call<R> call, Response<R> response) {
                    if (response.isSuccessful()) {
                        promise.complete(response.body());
                    } else {
                        promise.completeExceptionally(new Exception(response.errorBody().toString()));
                    }
                }

                @Override
                public void onFailure(Call<R> call, Throwable t) {
                    promise.completeExceptionally(t);
                }

            });

            return promise;
        }
    }
}
