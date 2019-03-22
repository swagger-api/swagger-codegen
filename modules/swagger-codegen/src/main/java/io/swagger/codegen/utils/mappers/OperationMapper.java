package io.swagger.codegen.utils.mappers;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import io.swagger.codegen.CodegenOperation;

@Mapper
public interface OperationMapper {

  public static OperationMapper INSTANCE = Mappers.getMapper(OperationMapper.class);

  CodegenOperation map(CodegenOperation operation);

}
