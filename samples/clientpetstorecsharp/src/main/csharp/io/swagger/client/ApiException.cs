using System;
using System.Runtime.Serialization;

namespace io.swagger.client 
{
    [Serializable]
    public class ApiException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //
        private readonly int _errorCode = 0;

        public ApiException()
        {
        }

        public ApiException(int errorCode, string message)
            : base(message)
        {
            _errorCode = errorCode;
        }
        
        public ApiException(int errorCode, Exception inner)
            :base("Failed Response", inner)
        {
            _errorCode = errorCode;
        }

        public ApiException(string message, Exception inner)
            : base(message, inner)
        {
        }

        protected ApiException(
            SerializationInfo info,
            StreamingContext context)
            : base(info, context)
        {
        }

        public int ErrorCode
        {
            get
            {
                return _errorCode;
            }
        }
    }
}