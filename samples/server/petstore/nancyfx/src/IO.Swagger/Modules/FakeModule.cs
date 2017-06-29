using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using IO.Swagger.v2.Models;
using IO.Swagger.v2.Utils;
using NodaTime;

namespace IO.Swagger.v2.Modules
{ 
    /// <summary>
    /// Form parameter enum test (string array)
    /// </summary>
    public enum TestEnumParametersEnumFormStringArrayEnum
    {
        >, 
        $
    };

    /// <summary>
    /// Form parameter enum test (string)
    /// </summary>
    public enum TestEnumParametersEnumFormStringEnum
    {
        _abc, 
        -efg, 
        (xyz)
    };

    /// <summary>
    /// Header parameter enum test (string array)
    /// </summary>
    public enum TestEnumParametersEnumHeaderStringArrayEnum
    {
        >, 
        $
    };

    /// <summary>
    /// Header parameter enum test (string)
    /// </summary>
    public enum TestEnumParametersEnumHeaderStringEnum
    {
        _abc, 
        -efg, 
        (xyz)
    };

    /// <summary>
    /// Query parameter enum test (string array)
    /// </summary>
    public enum TestEnumParametersEnumQueryStringArrayEnum
    {
        >, 
        $
    };

    /// <summary>
    /// Query parameter enum test (string)
    /// </summary>
    public enum TestEnumParametersEnumQueryStringEnum
    {
        _abc, 
        -efg, 
        (xyz)
    };

    /// <summary>
    /// Query parameter enum test (double)
    /// </summary>
    public enum TestEnumParametersEnumQueryIntegerEnum
    {
        1, 
        -2
    };

    /// <summary>
    /// Query parameter enum test (double)
    /// </summary>
    public enum TestEnumParametersEnumQueryDoubleEnum
    {
        1.1, 
        -1.2
    };


    /// <summary>
    /// Module processing requests of Fake domain.
    /// </summary>
    public sealed class FakeModule : NancyModule
    {
        /// <summary>
        /// Sets up HTTP methods mappings.
        /// </summary>
        /// <param name="service">Service handling requests</param>
        public FakeModule(FakeService service) : base("/v2")
        { 
            Patch["/fake"] = parameters =>
            {
                var body = this.Bind<Client>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'TestClientModel'");
                
                return service.TestClientModel(Context, body);
            };

            Post["/fake"] = parameters =>
            {
                var number = Parameters.ValueOf<decimal?>(parameters, Context.Request, "number", ParameterType.Undefined);
                var double = Parameters.ValueOf<double?>(parameters, Context.Request, "double", ParameterType.Undefined);
                var patternWithoutDelimiter = Parameters.ValueOf<string>(parameters, Context.Request, "patternWithoutDelimiter", ParameterType.Undefined);
                var byte = Parameters.ValueOf<byte[]>(parameters, Context.Request, "byte", ParameterType.Undefined);
                var integer = Parameters.ValueOf<int?>(parameters, Context.Request, "integer", ParameterType.Undefined);
                var int32 = Parameters.ValueOf<int?>(parameters, Context.Request, "int32", ParameterType.Undefined);
                var int64 = Parameters.ValueOf<long?>(parameters, Context.Request, "int64", ParameterType.Undefined);
                var float = Parameters.ValueOf<float?>(parameters, Context.Request, "float", ParameterType.Undefined);
                var string = Parameters.ValueOf<string>(parameters, Context.Request, "string", ParameterType.Undefined);
                var binary = Parameters.ValueOf<byte[]>(parameters, Context.Request, "binary", ParameterType.Undefined);
                var date = Parameters.ValueOf<ZonedDateTime?>(parameters, Context.Request, "date", ParameterType.Undefined);
                var dateTime = Parameters.ValueOf<ZonedDateTime?>(parameters, Context.Request, "dateTime", ParameterType.Undefined);
                var password = Parameters.ValueOf<string>(parameters, Context.Request, "password", ParameterType.Undefined);
                var callback = Parameters.ValueOf<string>(parameters, Context.Request, "callback", ParameterType.Undefined);
                Preconditions.IsNotNull(number, "Required parameter: 'number' is missing at 'TestEndpointParameters'");
                
                Preconditions.IsNotNull(double, "Required parameter: 'double' is missing at 'TestEndpointParameters'");
                
                Preconditions.IsNotNull(patternWithoutDelimiter, "Required parameter: 'patternWithoutDelimiter' is missing at 'TestEndpointParameters'");
                
                Preconditions.IsNotNull(byte, "Required parameter: 'byte' is missing at 'TestEndpointParameters'");
                
                service.TestEndpointParameters(Context, number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback);
                return new Response { ContentType = "application/xml; charset&#x3D;utf-8"};
            };

            Get["/fake"] = parameters =>
            {
                var enumFormStringArray = Parameters.ValueOf<TestEnumParametersEnumFormStringArrayEnum?>(parameters, Context.Request, "enumFormStringArray", ParameterType.Undefined);
                var enumFormString = Parameters.ValueOf<TestEnumParametersEnumFormStringEnum?>(parameters, Context.Request, "enumFormString", ParameterType.Undefined);
                var enumHeaderStringArray = Parameters.ValueOf<TestEnumParametersEnumHeaderStringArrayEnum?>(parameters, Context.Request, "enumHeaderStringArray", ParameterType.Header);
                var enumHeaderString = Parameters.ValueOf<TestEnumParametersEnumHeaderStringEnum?>(parameters, Context.Request, "enumHeaderString", ParameterType.Header);
                var enumQueryStringArray = Parameters.ValueOf<TestEnumParametersEnumQueryStringArrayEnum?>(parameters, Context.Request, "enumQueryStringArray", ParameterType.Query);
                var enumQueryString = Parameters.ValueOf<TestEnumParametersEnumQueryStringEnum?>(parameters, Context.Request, "enumQueryString", ParameterType.Query);
                var enumQueryInteger = Parameters.ValueOf<TestEnumParametersEnumQueryIntegerEnum?>(parameters, Context.Request, "enumQueryInteger", ParameterType.Query);
                var enumQueryDouble = Parameters.ValueOf<TestEnumParametersEnumQueryDoubleEnum?>(parameters, Context.Request, "enumQueryDouble", ParameterType.Undefined);
                service.TestEnumParameters(Context, enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
                return new Response { ContentType = "*/*"};
            };
        }
    }

    /// <summary>
    /// Service handling Fake requests.
    /// </summary>
    public interface FakeService
    {
        /// <summary>
        /// To test \&quot;client\&quot; model
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">client model</param>
        /// <returns>Client</returns>
        Client TestClientModel(NancyContext context, Client body);

        /// <summary>
        /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="number">None</param>
        /// <param name="double">None</param>
        /// <param name="patternWithoutDelimiter">None</param>
        /// <param name="byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="float">None (optional)</param>
        /// <param name="string">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <param name="callback">None (optional)</param>
        /// <returns></returns>
        void TestEndpointParameters(NancyContext context, decimal? number, double? double, string patternWithoutDelimiter, byte[] byte, int? integer, int? int32, long? int64, float? float, string string, byte[] binary, ZonedDateTime? date, ZonedDateTime? dateTime, string password, string callback);

        /// <summary>
        /// To test enum parameters
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="enumFormStringArray">Form parameter enum test (string array) (optional)</param>
        /// <param name="enumFormString">Form parameter enum test (string) (optional, default to -efg)</param>
        /// <param name="enumHeaderStringArray">Header parameter enum test (string array) (optional)</param>
        /// <param name="enumHeaderString">Header parameter enum test (string) (optional, default to -efg)</param>
        /// <param name="enumQueryStringArray">Query parameter enum test (string array) (optional)</param>
        /// <param name="enumQueryString">Query parameter enum test (string) (optional, default to -efg)</param>
        /// <param name="enumQueryInteger">Query parameter enum test (double) (optional)</param>
        /// <param name="enumQueryDouble">Query parameter enum test (double) (optional)</param>
        /// <returns></returns>
        void TestEnumParameters(NancyContext context, TestEnumParametersEnumFormStringArrayEnum? enumFormStringArray, TestEnumParametersEnumFormStringEnum? enumFormString, TestEnumParametersEnumHeaderStringArrayEnum? enumHeaderStringArray, TestEnumParametersEnumHeaderStringEnum? enumHeaderString, TestEnumParametersEnumQueryStringArrayEnum? enumQueryStringArray, TestEnumParametersEnumQueryStringEnum? enumQueryString, TestEnumParametersEnumQueryIntegerEnum? enumQueryInteger, TestEnumParametersEnumQueryDoubleEnum? enumQueryDouble);
    }

    /// <summary>
    /// Abstraction of FakeService.
    /// </summary>
    public abstract class AbstractFakeService: FakeService
    {
        public virtual Client TestClientModel(NancyContext context, Client body)
        {
            return TestClientModel(body);
        }

        public virtual void TestEndpointParameters(NancyContext context, decimal? number, double? double, string patternWithoutDelimiter, byte[] byte, int? integer, int? int32, long? int64, float? float, string string, byte[] binary, ZonedDateTime? date, ZonedDateTime? dateTime, string password, string callback)
        {
            TestEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback);
        }

        public virtual void TestEnumParameters(NancyContext context, TestEnumParametersEnumFormStringArrayEnum? enumFormStringArray, TestEnumParametersEnumFormStringEnum? enumFormString, TestEnumParametersEnumHeaderStringArrayEnum? enumHeaderStringArray, TestEnumParametersEnumHeaderStringEnum? enumHeaderString, TestEnumParametersEnumQueryStringArrayEnum? enumQueryStringArray, TestEnumParametersEnumQueryStringEnum? enumQueryString, TestEnumParametersEnumQueryIntegerEnum? enumQueryInteger, TestEnumParametersEnumQueryDoubleEnum? enumQueryDouble)
        {
            TestEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
        }

        protected abstract Client TestClientModel(Client body);

        protected abstract void TestEndpointParameters(decimal? number, double? double, string patternWithoutDelimiter, byte[] byte, int? integer, int? int32, long? int64, float? float, string string, byte[] binary, ZonedDateTime? date, ZonedDateTime? dateTime, string password, string callback);

        protected abstract void TestEnumParameters(TestEnumParametersEnumFormStringArrayEnum? enumFormStringArray, TestEnumParametersEnumFormStringEnum? enumFormString, TestEnumParametersEnumHeaderStringArrayEnum? enumHeaderStringArray, TestEnumParametersEnumHeaderStringEnum? enumHeaderString, TestEnumParametersEnumQueryStringArrayEnum? enumQueryStringArray, TestEnumParametersEnumQueryStringEnum? enumQueryString, TestEnumParametersEnumQueryIntegerEnum? enumQueryInteger, TestEnumParametersEnumQueryDoubleEnum? enumQueryDouble);
    }

}
