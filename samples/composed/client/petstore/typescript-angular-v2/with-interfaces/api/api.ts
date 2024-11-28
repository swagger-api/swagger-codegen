export * from './default.service';
import { DefaultService } from './default.service';
export * from './default.serviceInterface'
export * from './pet.service';
import { PetService } from './pet.service';
export * from './pet.serviceInterface'
export * from './store.service';
import { StoreService } from './store.service';
export * from './store.serviceInterface'
export * from './user.service';
import { UserService } from './user.service';
export * from './user.serviceInterface'
export const APIS = [DefaultService, PetService, StoreService, UserService];
