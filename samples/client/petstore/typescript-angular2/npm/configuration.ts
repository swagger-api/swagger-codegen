export class Configuration {
    apiKeys: {[ key: string ]: string};
    username: string;
    password: string;
    accessToken: string | (() => string);
}
