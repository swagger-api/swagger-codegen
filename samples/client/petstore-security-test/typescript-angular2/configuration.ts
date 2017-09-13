import { Observable } from 'rxjs/Observable';

export interface ConfigurationParameters {
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    accessToken?: string | ((name: string, scopes?: string[]) => Observable<string>);
    basePath?: string;
    withCredentials?: boolean;
}

export class Configuration {
    apiKeys?: {[ key: string ]: string};
    username?: string;
    password?: string;
    accessToken?: string | ((name: string, scopes?: string[]) => Observable<string>);
    basePath?: string;
    withCredentials?: boolean;

    constructor(configurationParameters: ConfigurationParameters = {}) {
        this.apiKeys = configurationParameters.apiKeys;
        this.username = configurationParameters.username;
        this.password = configurationParameters.password;
        this.accessToken = configurationParameters.accessToken;
        this.basePath = configurationParameters.basePath;
        this.withCredentials = configurationParameters.withCredentials;
    }
}
