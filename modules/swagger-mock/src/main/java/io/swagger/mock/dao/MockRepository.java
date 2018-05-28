package io.swagger.mock.dao;

import org.springframework.data.repository.CrudRepository;

import io.swagger.mock.entity.MockEntity;

public interface MockRepository extends CrudRepository<MockEntity, Long> {
}