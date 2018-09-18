import { HttpParams } from '@angular/common/http';

export interface ConfigurationParameters {
    /**
     * the path that will be used to talk to the ordercloud api
     * this defaults to https://api.ordercloud.io/v1 
     * 
     * it may be useful to change this to interact with different
     * environments or different versions of the api
     * 
     * at the time of writing there is only one version of the api
     * 
     */
    basePath?: string;
    /**
     * the path that will be used to authenticate to the ordercloud api
     * this defaults to https://auth.ordercloud.io/oauth/token
     * 
     * if may be useful to change this to interact with different
     * environments
     * 
     */
    authPath?: string;
    /**
     * this cookie prefix will be added to the name of any tokens created
     * in the sdk check out the token service for all methods
     */
    cookiePrefix?: string;
}

export class Configuration {
    basePath?: string;
    authPath?: string;
    cookiePrefix?: string;

    constructor(configurationParameters: ConfigurationParameters = {}) {
        this.basePath = configurationParameters.basePath;
        this.authPath = configurationParameters.authPath;
        this.cookiePrefix = configurationParameters.cookiePrefix;
    }

    /**
     * Unwrap filters object - Swagger doesn't support free-form query parameters
     * out of the box so we need to manually unwrap filters ourselves
     */
    public unwrapFilters(filters: any, queryParameters: HttpParams, operationId: string) {
        for (var filterKey in filters) {
            var filterVal = filters[filterKey];
              if(filterVal === null) {
                  throw new Error(`Parameter ${filterKey} was null when calling ${operationId}. Use negative filters ("!" operator) to return items without a certain value. For example an order list call with status: !Unsubmitted would return all orders without a status of Unsubmitted`);
              }
              if (filters.hasOwnProperty(filterKey) && filterVal !== undefined) {
                if (filterVal instanceof Array) {
                    filterVal.forEach((arrayVal, index) => {
                        let action = index === 0 ? 'set' : 'append';
                        queryParameters = queryParameters[action](filterKey, <any>arrayVal);
                    })
                } else {
                    queryParameters = queryParameters.set(filterKey, filterVal);
                }
              }
            }
        return queryParameters;
    }

    /**
     * Select the correct content-type to use for a request.
     * Uses {@link Configuration#isJsonMime} to determine the correct content-type.
     * If no content type is found return the first found type if the contentTypes is not empty
     * @param {string[]} contentTypes - the array of content types that are available for selection
     * @returns {string} the selected content-type or <code>undefined</code> if no selection could be made.
     */
    public selectHeaderContentType (contentTypes: string[]): string | undefined {
        if (contentTypes.length == 0) {
            return undefined;
        }

        let type = contentTypes.find(x => this.isJsonMime(x));
        if (type === undefined) {
            return contentTypes[0];
        }
        return type;
    }

    /**
     * Select the correct accept content-type to use for a request.
     * Uses {@link Configuration#isJsonMime} to determine the correct accept content-type.
     * If no content type is found return the first found type if the contentTypes is not empty
     * @param {string[]} accepts - the array of content types that are available for selection.
     * @returns {string} the selected content-type or <code>undefined</code> if no selection could be made.
     */
    public selectHeaderAccept(accepts: string[]): string | undefined {
        if (accepts.length == 0) {
            return undefined;
        }

        let type = accepts.find(x => this.isJsonMime(x));
        if (type === undefined) {
            return accepts[0];
        }
        return type;
    }

    /**
     * Check if the given MIME is a JSON MIME.
     * JSON MIME examples:
     *   application/json
     *   application/json; charset=UTF8
     *   APPLICATION/JSON
     *   application/vnd.company+json
     * @param {string} mime - MIME (Multipurpose Internet Mail Extensions)
     * @return {boolean} True if the given MIME is JSON, false otherwise.
     */
    public isJsonMime(mime: string): boolean {
        const jsonMime: RegExp = new RegExp('^(application\/json|[^;/ \t]+\/[^;/ \t]+[+]json)[ \t]*(;.*)?$', 'i');
        return mime != null && (jsonMime.test(mime) || mime.toLowerCase() === 'application/json-patch+json');
    }
}
