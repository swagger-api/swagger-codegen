using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Model for testing reserved words
    /// </summary>
    public sealed class Return:  IEquatable<Return>
    { 
        /// <summary>
        /// _Return
        /// </summary>
        public int? _Return { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Return.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Return()
        {
        }

        private Return(int? _Return)
        {
            
            this._Return = _Return;
            
        }

        /// <summary>
        /// Returns builder of Return.
        /// </summary>
        /// <returns>ReturnBuilder</returns>
        public static ReturnBuilder Builder()
        {
            return new ReturnBuilder();
        }

        /// <summary>
        /// Returns ReturnBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ReturnBuilder</returns>
        public ReturnBuilder With()
        {
            return Builder()
                ._Return(_Return);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Return other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Return.
        /// </summary>
        /// <param name="left">Compared (Return</param>
        /// <param name="right">Compared (Return</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Return left, Return right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Return.
        /// </summary>
        /// <param name="left">Compared (Return</param>
        /// <param name="right">Compared (Return</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Return left, Return right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Return.
        /// </summary>
        public sealed class ReturnBuilder
        {
            private int? __Return;

            internal ReturnBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Return._Return property.
            /// </summary>
            /// <param name="value">_Return</param>
            public ReturnBuilder _Return(int? value)
            {
                __Return = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Return.
            /// </summary>
            /// <returns>Return</returns>
            public Return Build()
            {
                Validate();
                return new Return(
                    _Return: __Return
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
