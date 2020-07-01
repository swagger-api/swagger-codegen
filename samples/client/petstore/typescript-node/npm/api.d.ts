/// <reference types="request" />
/// <reference types="bluebird" />
/// <reference types="node" />
import localVarRequest = require('request');
import http = require('http');
import Promise = require('bluebird');
export declare class Amount {
    'value': number;
    'currency': Currency;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class ApiResponse {
    'code'?: number;
    'type'?: string;
    'message'?: string;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class Category {
    'id'?: number;
    'name'?: string;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class Currency {
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class Order {
    'id'?: number;
    'petId'?: number;
    'quantity'?: number;
    'shipDate'?: Date;
    'status'?: Order.StatusEnum;
    'complete'?: boolean;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare namespace Order {
    enum StatusEnum {
        Placed,
        Approved,
        Delivered,
    }
}
export declare class Pet {
    'id'?: number;
    'category'?: Category;
    'name': string;
    'photoUrls': Array<string>;
    'tags'?: Array<Tag>;
    'status'?: Pet.StatusEnum;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare namespace Pet {
    enum StatusEnum {
        Available,
        Pending,
        Sold,
    }
}
export declare class Tag {
    'id'?: number;
    'name'?: string;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class User {
    'id'?: number;
    'username'?: string;
    'firstName'?: string;
    'lastName'?: string;
    'email'?: string;
    'password'?: string;
    'phone'?: string;
    'userStatus'?: number;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export interface Authentication {
    applyToRequest(requestOptions: localVarRequest.Options): void;
}
export declare class HttpBasicAuth implements Authentication {
    username: string;
    password: string;
    applyToRequest(requestOptions: localVarRequest.Options): void;
}
export declare class ApiKeyAuth implements Authentication {
    private location;
    private paramName;
    apiKey: string;
    constructor(location: string, paramName: string);
    applyToRequest(requestOptions: localVarRequest.Options): void;
}
export declare class OAuth implements Authentication {
    accessToken: string;
    applyToRequest(requestOptions: localVarRequest.Options): void;
}
export declare class VoidAuth implements Authentication {
    username: string;
    password: string;
    applyToRequest(_: localVarRequest.Options): void;
}
export declare enum PetApiApiKeys {
    api_key = 0,
}
export declare class PetApi {
    protected _basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    basePath: string;
    setDefaultAuthentication(auth: Authentication): void;
    setApiKey(key: PetApiApiKeys, value: string): void;
    accessToken: string;
    addPet(body: Pet, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    deletePet(petId: number, apiKey?: string, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): Promise<{
        response: http.ClientResponse;
        body: Array<Pet>;
    }>;
    findPetsByTags(tags: Array<string>, options?: any): Promise<{
        response: http.ClientResponse;
        body: Array<Pet>;
    }>;
    getPetById(petId: number, options?: any): Promise<{
        response: http.ClientResponse;
        body: Pet;
    }>;
    updatePet(body: Pet, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    updatePetWithForm(petId: number, name?: string, status?: string, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    uploadFile(petId: number, additionalMetadata?: string, file?: Buffer, options?: any): Promise<{
        response: http.ClientResponse;
        body: ApiResponse;
    }>;
}
export declare enum StoreApiApiKeys {
    api_key = 0,
}
export declare class StoreApi {
    protected _basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    basePath: string;
    setDefaultAuthentication(auth: Authentication): void;
    setApiKey(key: StoreApiApiKeys, value: string): void;
    accessToken: string;
    deleteOrder(orderId: string, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    getInventory(options?: any): Promise<{
        response: http.ClientResponse;
        body: {
            [key: string]: number;
        };
    }>;
    getOrderById(orderId: number, options?: any): Promise<{
        response: http.ClientResponse;
        body: Order;
    }>;
    placeOrder(body: Order, options?: any): Promise<{
        response: http.ClientResponse;
        body: Order;
    }>;
}
export declare enum UserApiApiKeys {
    api_key = 0,
}
export declare class UserApi {
    protected _basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    basePath: string;
    setDefaultAuthentication(auth: Authentication): void;
    setApiKey(key: UserApiApiKeys, value: string): void;
    accessToken: string;
    createUser(body: User, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    createUsersWithArrayInput(body: Array<User>, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    createUsersWithListInput(body: Array<User>, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    deleteUser(username: string, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    getUserByName(username: string, options?: any): Promise<{
        response: http.ClientResponse;
        body: User;
    }>;
    loginUser(username: string, password: string, options?: any): Promise<{
        response: http.ClientResponse;
        body: string;
    }>;
    logoutUser(options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    updateUser(username: string, body: User, options?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
}
