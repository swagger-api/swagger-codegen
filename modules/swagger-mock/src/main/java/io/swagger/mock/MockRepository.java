package io.swagger.mock;

import org.springframework.data.repository.CrudRepository;

import io.swagger.mock.entity.MockEntity;

public interface MockRepository extends CrudRepository<MockEntity, Long> {
}