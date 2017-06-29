using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Model for testing model name starting with number
    /// </summary>
    public sealed class Model200Response:  IEquatable<Model200Response>
    { 
        /// <summary>
        /// Name
        /// </summary>
        public int? Name { get; private set; }

        /// <summary>
        /// Class
        /// </summary>
        public string Class { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Model200Response.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Model200Response()
        {
        }

        private Model200Response(int? Name, string Class)
        {
            
            this.Name = Name;
            
            this.Class = Class;
            
        }

        /// <summary>
        /// Returns builder of Model200Response.
        /// </summary>
        /// <returns>Model200ResponseBuilder</returns>
        public static Model200ResponseBuilder Builder()
        {
            return new Model200ResponseBuilder();
        }

        /// <summary>
        /// Returns Model200ResponseBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>Model200ResponseBuilder</returns>
        public Model200ResponseBuilder With()
        {
            return Builder()
                .Name(Name)
                .Class(Class);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Model200Response other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Model200Response.
        /// </summary>
        /// <param name="left">Compared (Model200Response</param>
        /// <param name="right">Compared (Model200Response</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Model200Response left, Model200Response right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Model200Response.
        /// </summary>
        /// <param name="left">Compared (Model200Response</param>
        /// <param name="right">Compared (Model200Response</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Model200Response left, Model200Response right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Model200Response.
        /// </summary>
        public sealed class Model200ResponseBuilder
        {
            private int? _Name;
            private string _Class;

            internal Model200ResponseBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Model200Response.Name property.
            /// </summary>
            /// <param name="value">Name</param>
            public Model200ResponseBuilder Name(int? value)
            {
                _Name = value;
                return this;
            }

            /// <summary>
            /// Sets value for Model200Response.Class property.
            /// </summary>
            /// <param name="value">Class</param>
            public Model200ResponseBuilder Class(string value)
            {
                _Class = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Model200Response.
            /// </summary>
            /// <returns>Model200Response</returns>
            public Model200Response Build()
            {
                Validate();
                return new Model200Response(
                    Name: _Name,
                    Class: _Class
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
